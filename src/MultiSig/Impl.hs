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
    dup
    preloadFromSlice @Nonce
    pushInt 0
    if IsEq
      then do
        comment "Handling init message"
        drop
        pushRoot
        decodeFromCell @Storage
        moveOnTop @4
        pushInt 0
        if IsEq
          then do
            comment "State wasn't yet initialized"
            accept
            stacktype @'[TimestampDict, OrderDict, DSet PublicKey, Word32]
            reversePrefix @4
            pushInt 1
            encodeToCell @Storage
            popRoot
          else do
            comment "State was already initialized, rejecting"
            drop
            drop
            drop
            drop
      else do
        decodeFromSlice @SignMsg
        endS
        recvSignMsg

recvSignMsg :: DecodeSliceFields SignMsg :-> '[]
recvSignMsg = viaSubroutine @(DecodeSliceFields SignMsg) @'[] "recvSignMsg" $ do
    comment "Handle sign message"
    -- Garbage collection of expired orders
    pushRoot
    decodeFromCell @Storage

    rollRev @7
    stacktype @[OrderDict, DSet PublicKey, Word32, Nonce, Cell SignMsgBody, SignDict, Nonce, TimestampDict]

    -- Check that nonces of the storage and the message matched
    comment "Checking that nonces match"
    moveOnTop @6
    push @4
    compareNonces
    throwIfNot NonceMismatch
    stacktype @[OrderDict, DSet PublicKey, Word32, Nonce, Cell SignMsgBody, SignDict, TimestampDict]

    -- Check that the message hasn't expired
    comment "Checking that the message hasn't expired"
    moveOnTop @4
    dup
    checkMsgExpiration
    throwIfNot MsgExpired

    -- Compute the message body hash
    comment "Compute hash of message body"
    dup
    computeMsgBodyHash
    stacktype @[Hash SignMsgBody, Cell SignMsgBody, OrderDict, DSet PublicKey, Word32, Nonce, SignDict, TimestampDict]

    -- Remove signatures of the message which are not valid
    comment "Filter invalid signature from the message"
    dup
    push @4
    swap
    moveOnTop @8
    filterValidSignatures
    stacktype @[AccumPkDict, Hash SignMsgBody, Cell SignMsgBody, OrderDict, DSet PublicKey, Word32, Nonce, TimestampDict]

    -- Add valid signatures to the storage's OrderDict
    push @5 -- push K on the top
    roll @8
    roll @8
    dup
    rollRev @9
    stacktype @[Nonce, TimestampDict, Word32, AccumPkDict, Hash SignMsgBody, Cell SignMsgBody, OrderDict, DSet PublicKey, Word32, Nonce]
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

-- | Return true if nonces equal.
compareNonces :: Nonce & Nonce & s :-> Bool & s
compareNonces = equalInt

-- | Fetch expiraiton time from Cell SignMsgBody
getExpirationTime :: Cell SignMsgBody & s :-> Timestamp & s
getExpirationTime = do
    decodeFromCell @SignMsgBody
    drop

-- | Returns True, if a passed timestamp hasn't expired.
checkMsgExpiration :: Cell SignMsgBody & s :-> Bool & s
checkMsgExpiration = do
    getExpirationTime
    now
    Proxy @Timestamp `greaterInt` Proxy @CurrentTimestamp

computeMsgBodyHash:: Cell SignMsgBody & s :-> Hash SignMsgBody & s
computeMsgBodyHash = cellHash

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
    push @2
    push @5
    dictGet
    if IsJust then do
        stacktype' @[Order, Word32]
        cast @Order @Slice
        decodeFromSliceFull @Order
        pop @1
        -- ^ drop MsgBody from storage because there is one from msg
        moveOnTop @2
        swap
        false -- not new one
        rollRev @3
        --                                                    v whether new order or not
    else do
        swap
        newDict
        true -- new one
        rollRev @3

    swap
    cast @AccumPkDict @(DSet PublicKey)
    swap
    dictMerge
    if IsJust then do
        -- when not enough signatures
        stacktype' @[DSet PublicKey, Bool, Hash SignMsgBody, Cell SignMsgBody, OrderDict, Nonce, TimestampDict]
        rollRev @4
        rollRev @4
        moveOnTop @3
        moveOnTop @2
        dup
        rollRev @7
        push @2
        rollRev @8
        stacktype' @[Cell SignMsgBody, DSet PublicKey, Hash SignMsgBody, OrderDict, Bool, Nonce, TimestampDict, Cell SignMsgBody, Hash SignMsgBody]
        dictEncodeSet @(Hash SignMsgBody) @Order
        rollRev @5
        roll @4
        roll @4
        roll @2
        stacktype' @[Bool, Cell SignMsgBody, Hash SignMsgBody,  Nonce, TimestampDict, OrderDict]
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
        stacktype' @[Timestamp, OrderDict, Bool, Nonce, TimestampDict]
        rollRev @2
        rollRev @4
        ifElse (drop >> drop) removeFromTimestampSet

-- Add to set to perform garbage collection effectively
addToTimestampSet :: Cell SignMsgBody & Hash SignMsgBody & Nonce & TimestampDict & s :-> TimestampDict & s
addToTimestampSet = do
    getExpirationTime
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
    dictIter $ do
      stacktype' @'[DSet PublicKey, Cell SignMsgBody, Hash SignMsgBody, OrderDict, AccumOrderDict, Bool, PublicKey]
      push @5
      push @7
      push @2
      checkSignMsgBodyBelongsToPk
      ifElse
        (do
          stacktype' @'[DSet PublicKey, Cell SignMsgBody, Hash SignMsgBody, OrderDict, AccumOrderDict]
          moveOnTop @3
          moveOnTop @4
          cast @AccumOrderDict @OrderDict
          rollRev @4
          rollRev @4
          stacktype' @'[DSet PublicKey, Cell SignMsgBody, Hash SignMsgBody, {-Accum-}OrderDict, OrderDict]
          swap
          dictEncodeSet
          cast @OrderDict @AccumOrderDict
          swap
        )
        (drop >> drop >> drop)
    pop @1
    pop @1

checkSignMsgBodyBelongsToPk
  :: DSet PublicKey & PublicKey & Bool & s :-> Bool & s
checkSignMsgBodyBelongsToPk = do
    dsetGet
    equalInt
