{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module MultiSig.Impl
       ( recvExternal
       , recvInternal

       , getAllOrders
       , getOrdersByKey
       ) where

import Prelude

import MultiSig.Types
import FiftAsm


recvInternal :: '[Slice] :-> '[]
recvInternal = drop

recvExternal :: '[Slice] :-> '[]
recvExternal = do
    pushRoot
    decodeFromCell @Storage
    stacktype @'[TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce, Slice]
    push @4
    pushInt 0
    if IsEq
      then do
        comment "Handling init message"
        accept
        moveOnTop @4
        drop
        reversePrefix @4
        pushInt 1
        encodeToCell @Storage
        popRoot
        drop
      else
        recvSignMsg

recvSignMsg
  :: forall xs.
    xs ~ '[TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce, Slice]
  => xs :-> '[]
recvSignMsg = viaSubroutine @xs @'[] "recvSignMsg" $ do
    comment "Handle sign message"

    moveOnTop @5
    decodeFromSlice @SignMsg
    endS

    stacktype @'[Cell SignMsgBody, SignDict, TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce]

    dup
    decodeFromCell @SignMsgBody

    stacktype @[Cell MessageObject, Timestamp, Nonce, Cell SignMsgBody, SignDict, TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Check that nonces of the storage and the message matched
    comment "Checking that nonces match"
    reversePrefix @3
    push @9
    equalInt @Nonce
    throwIfNot NonceMismatch

    stacktype' @'[Timestamp]

    -- Check that the message hasn't expired
    comment "Checking that the message hasn't expired"
    now
    Proxy @Timestamp `greaterInt` Proxy @CurrentTimestamp
    throwIfNot MsgExpired

    stacktype' @[Cell MessageObject, Cell SignMsgBody]

    -- Compute the message body hash
    comment "Compute hash of message body"
    push @1
    cellHash

    stacktype @[Hash SignMsgBody, Cell MessageObject, Cell SignMsgBody, SignDict, TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Remove signatures of the message which are not valid
    comment "Filter invalid signature from the message"
    dup
    push @7
    swap
    moveOnTop @5
    filterValidSignatures

    stacktype @[AccumPkDict, Hash SignMsgBody, Cell MessageObject, Cell SignMsgBody, TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce]

    moveOnTop @2
    drop
    push @6 -- push K on the top

    stacktype @[Word32, AccumPkDict, Hash SignMsgBody, Cell SignMsgBody, TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce]

    moveOnTop @4
    push @8

    extendOrder
    stacktype @[TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce]

    garbageCollectOrders

    comment "Encode storage fields"
    reversePrefix @5 -- reverse first 4 elements
    stacktype @[Nonce, Word32, DSet PublicKey, OrderDict, TimestampDict]
    inc
    encodeToCell @Storage
    popRoot

data IterTimestampDict
type instance ToTVM IterTimestampDict = ToTVM TimestampDict

-- Garbage collection of expired orders
garbageCollectOrders :: TimestampDict & OrderDict & s :-> TimestampDict & OrderDict & s
garbageCollectOrders =
  viaSubroutine @'[TimestampDict, OrderDict]  @'[TimestampDict, OrderDict] "garbageCollectOrders" $ do
    now
    swap
    dup
    dictIter $ do
        stacktype' @[Hash SignMsgBody, TimeNonce, TimestampDict, TimestampDict, CurrentTimestamp, OrderDict]
        cast2 @TimestampDict @IterTimestampDict
        swap
        unpackTime
        push @4
        if Proxy @Timestamp <: Proxy @CurrentTimestamp then do
            -- If an order has been expired
            -- 1. Remove a hash from OrderDict
            moveOnTop @4
            stacktype @'[OrderDict, Hash SignMsgBody, IterTimestampDict, TimestampDict, CurrentTimestamp]
            dictDelIgnore
            rollRev @3
            stacktype @'[IterTimestampDict, TimestampDict, CurrentTimestamp, OrderDict]
            -- 2. Copy new TimestampDict where an element already removed
            pop @1
            dup
            cast1 @IterTimestampDict @TimestampDict
            cast @IterTimestampDict @TimestampDict
        else do
            -- Push empty dictionary to stop iterating
            drop >> drop
            newDict @TimeNonce @(Hash SignMsgBody)
    pop @1

data AccumPkDict
type instance ToTVM AccumPkDict = ToTVM (DSet PublicKey)

filterValidSignatures :: SignDict & Hash SignMsgBody & DSet PublicKey & s :-> AccumPkDict & s
filterValidSignatures =
  viaSubroutine @'[SignDict, Hash SignMsgBody, DSet PublicKey] @'[AccumPkDict] "filterValidSignatures" $ do
    newDict
    cast @(DSet PublicKey) @AccumPkDict
    swap
    dictIter $ do
        stacktype' @[Signature, PublicKey, SignDict, AccumPkDict, Hash SignMsgBody, DSet PublicKey]
        swap
        dup
        push @6
        dsetGet
        if NotHolds then
            drop >> drop
        else do
            stacktype' @[PublicKey, Signature, SignDict, AccumPkDict, Hash SignMsgBody, DSet PublicKey]
            dup
            rollRev @2
            push @5
            rollRev @2
            stacktype' @[PublicKey, Signature, Hash SignMsgBody]
            chkSignU
            stacktype' @[Bool, PublicKey, SignDict, AccumPkDict, Hash SignMsgBody, DSet PublicKey]
            throwIfNot InvalidSignature
            accept
            moveOnTop @2
            cast @AccumPkDict @(DSet PublicKey)
            dsetSet
            cast @(DSet PublicKey) @AccumPkDict
            swap
    pop @1
    pop @1


extendOrder
    :: Nonce & TimestampDict & Word32 & AccumPkDict & Hash SignMsgBody & Cell SignMsgBody & OrderDict & s
    :-> TimestampDict & OrderDict & s
extendOrder =
  viaSubroutine
      @'[Nonce, TimestampDict, Word32, AccumPkDict, Hash SignMsgBody, Cell SignMsgBody, OrderDict]
      @'[TimestampDict, OrderDict] "extendOrder" $ do
    rollRev @6
    rollRev @6
    stacktype' @[Word32, AccumPkDict, Hash SignMsgBody, Cell SignMsgBody, OrderDict, Nonce, TimestampDict]
    push @2
    push @5
    stacktype' @[OrderDict, Hash SignMsgBody, Word32, AccumPkDict]
    dictGet
    if IsJust then do
        stacktype' @[DSet PublicKey, Word32]
        false -- not new one
    else do
        newDict
        true -- new one

    stacktype' @[Bool, DSet PublicKey, Word32, AccumPkDict]

    rollRev @3
    moveOnTop @2

    stacktype' @[AccumPkDict, DSet PublicKey, Word32, Bool]
    cast @AccumPkDict @(DSet PublicKey)

    dictMerge

    if IsJust then do
        -- when not enough signatures
        stacktype' @[DSet PublicKey, Bool, Hash SignMsgBody, Cell SignMsgBody, OrderDict, Nonce, TimestampDict]
        moveOnTop @4
        push @3
        moveOnTop @2
        dictEncodeSet @(Hash SignMsgBody) @(DSet PublicKey)

        stacktype @'[OrderDict, Bool, Hash SignMsgBody, Cell SignMsgBody, Nonce, TimestampDict]

        rollRev @5

        stacktype @'[Bool, Hash SignMsgBody, Cell SignMsgBody, Nonce, TimestampDict, OrderDict]
        ifElse addToTimestampSet (drop >> drop >> drop)
    else do
        stacktype' @[Bool, Hash SignMsgBody, Cell SignMsgBody, OrderDict, Nonce, TimestampDict]
        swap
        moveOnTop @3
        dictDelIgnore
        moveOnTop @2
        stacktype' @[Cell SignMsgBody, OrderDict, Bool, Nonce, TimestampDict]
        decodeFromCell @SignMsgBody
        pushInt 0 -- msg type = 0
        sendRawMsg
        pop @1
        stacktype' @[Timestamp, OrderDict, Bool, Nonce, TimestampDict]
        rollRev @2
        rollRev @4
        ifElse (drop >> drop) removeFromTimestampSet

-- Add to set to perform garbage collection effectively
addToTimestampSet :: Hash SignMsgBody & Cell SignMsgBody & Nonce & TimestampDict & s :-> TimestampDict & s
addToTimestampSet = do
    swap
    decodeFromCell @SignMsgBody
    drop
    pop @1
    moveOnTop @2
    swap
    timeNoncePack
    stacktype' @[TimeNonce, Hash SignMsgBody, TimestampDict]
    roll @2
    dictSet

-- Remove from set expired entry
removeFromTimestampSet :: Timestamp & Nonce & TimestampDict & s :-> TimestampDict & s
removeFromTimestampSet = do
    timeNoncePack
    swap
    dictDelIgnore

--
-- Getter methods
--

data AccumOrderDict
type instance ToTVM AccumOrderDict = ToTVM OrderDict


getAllOrders :: '[] :-> '[AccumOrderDict]
getAllOrders = do
    comment "Get all orders"
    pushRoot
    decodeFromCell @Storage
    drop
    rollRev @3
    drop >> drop >> drop
    cast @OrderDict @AccumOrderDict


getOrdersByKey :: '[Bool, PublicKey] :-> '[AccumOrderDict]
getOrdersByKey = do
    comment "Get orders by key"
    pushRoot
    decodeFromCell @Storage
    drop
    rollRev @3
    drop >> drop >> drop
    newDict
    cast @OrderDict @AccumOrderDict
    swap
    stacktype @'[OrderDict, AccumOrderDict, Bool, PublicKey]
    dictIter $ do
      stacktype @'[DSet PublicKey, Hash SignMsgBody, OrderDict, AccumOrderDict, Bool, PublicKey]
      dup
      stacktype @'[DSet PublicKey, DSet PublicKey, Hash SignMsgBody, OrderDict, AccumOrderDict, Bool, PublicKey]
      push @6
      push @6
      reversePrefix @3
      checkSignMsgBodyBelongsToPk
      stacktype @'[Bool, DSet PublicKey, Hash SignMsgBody, OrderDict, AccumOrderDict, Bool, PublicKey]
      ifElse
        (do
          stacktype @'[DSet PublicKey, Hash SignMsgBody, OrderDict, AccumOrderDict, Bool, PublicKey]
          moveOnTop @2
          moveOnTop @3
          cast @AccumOrderDict @OrderDict
          rollRev @3
          rollRev @3
          stacktype' @'[DSet PublicKey, Hash SignMsgBody, {-Accum-}OrderDict, OrderDict]
          dictEncodeSet
          cast @OrderDict @AccumOrderDict
          swap
        )
        (drop >> drop)
    pop @1
    pop @1

checkSignMsgBodyBelongsToPk
  :: DSet PublicKey & PublicKey & Bool & s :-> Bool & s
checkSignMsgBodyBelongsToPk = do
    dsetGet
    equalInt
