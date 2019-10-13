{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module MultiSig.Impl
       ( recvExternal
       ) where

import Prelude

import MultiSig.Types
import FiftAsm

recvExternal :: '[Slice] :-> '[]
recvExternal = decodeMsgFromSliceFull recvGetAllOrders recvGetOrdersByKey recvSignMsg

recvGetAllOrders :: '[] :-> '[]
recvGetAllOrders = viaSubroutine @'[] @'[] "recvGetAllOrders" $ do
    comment "Get all orders"
    pushRoot
    decodeFromCell @Storage
    roll @4
    drop >> drop >> drop
    cast @OrderDict @AccumOrderDict
    mkMethodReturnMessage
    sendRawMsg

mkMethodReturnMessage
  :: '[AccumOrderDict]
  :-> '[Word32, Cell MessageObject]
mkMethodReturnMessage =
  viaSubroutine @'[AccumOrderDict]
                @'[Word32, Cell MessageObject] "mkMethodReturnMessage" $ do
    cast @AccumOrderDict @OrderDict
    -- TODO replace with actual serialization
    encodeToCell @OrderDict
    cast @(Cell OrderDict) @(Cell MessageObject)
    pushInt 0

data AccumOrderDict
type instance ToTVM AccumOrderDict = ToTVM OrderDict

recvGetOrdersByKey :: '[Bool, PublicKey] :-> '[]
recvGetOrdersByKey = viaSubroutine @'[Bool, PublicKey] @'[] "recvGetOrdersByKey" $ do
    comment "Get orders by key"
    pushRoot
    decodeFromCell @Storage
    roll @4
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
          roll @5
          roll @5
          stacktype' @'[DSet PublicKey, Cell SignMsgBody, Hash SignMsgBody, {-Accum-}OrderDict, OrderDict]
          swap
          dictEncodeSet
          cast @OrderDict @AccumOrderDict
          swap
        )
        (drop >> drop >> drop)
    swap
    drop
    swap
    drop
    mkMethodReturnMessage
    sendRawMsg

checkSignMsgBodyBelongsToPk
  :: DSet PublicKey & PublicKey & Bool & s :-> Bool & s
checkSignMsgBodyBelongsToPk = do
    dsetGet
    if IsEq
      then true
      else false

recvSignMsg :: DecodeSliceFields SignMsg :-> '[]
recvSignMsg = viaSubroutine @(DecodeSliceFields SignMsg) @'[] "recvSignMsg" $ do
    comment "Handle sign message"
    -- Garbage collection of expired orders
    pushRoot
    decodeFromCell @Storage
    -- TODO store garbage collected OrderDict regardless
    -- of message processing
    garbageCollectOrders

    stacktype @[OrderDict, DSet PublicKey, Word32, Nonce, Cell SignMsgBody, SignDict, Nonce]

    -- Check that nonces of the storage and the message matched
    comment "Checking that nonces match"
    moveOnTop @6
    push @4
    compareNonces
    throwIfNot NonceMismatch
    stacktype @[OrderDict, DSet PublicKey, Word32, Nonce, Cell SignMsgBody, SignDict]

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
    stacktype @[Hash SignMsgBody, Cell SignMsgBody, OrderDict, DSet PublicKey, Word32, Nonce, SignDict]

    -- Remove signatures of the message which are not valid
    comment "Filter invalid signature from the message"
    dup
    push @4
    swap
    moveOnTop @8
    filterValidSignatures
    stacktype @[AccumPkDict, Hash SignMsgBody, Cell SignMsgBody, OrderDict, DSet PublicKey, Word32, Nonce]
    dup
    cast @AccumPkDict @(DSet PublicKey)
    dictEmpty
    throwIf NoValidSignatures

    -- Add valid signatures to the storage's OrderDict
    push @5
    extendOrder
    stacktype @[OrderDict, DSet PublicKey, Word32, Nonce]

    comment "Encode storage fields"
    reversePrefix @4 -- reverse first 4 elements
    stacktype @[Nonce, Word32, DSet PublicKey, OrderDict]
    inc
    encodeToCell @Storage
    popRoot

-- TODO Garbage collection of expired orders
garbageCollectOrders :: OrderDict & s :-> OrderDict & s
garbageCollectOrders = ignore

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
    greaterInt

computeMsgBodyHash:: Cell SignMsgBody & s :-> Hash SignMsgBody & s
computeMsgBodyHash = cellHash

data AccumPkDict
type instance ToTVM AccumPkDict = ToTVM (DSet PublicKey)

filterValidSignatures :: SignDict & Hash SignMsgBody & DSet PublicKey & s :-> AccumPkDict & s
filterValidSignatures = do
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
            roll @3
            push @5
            roll @3
            chkSignU
            stacktype' @[Bool, PublicKey, SignDict, AccumPkDict, Hash SignMsgBody, DSet PublicKey]
            if Holds then do
                moveOnTop @2
                cast @AccumPkDict @(DSet PublicKey)
                dsetSet
                cast @(DSet PublicKey) @AccumPkDict
                swap
            else
                drop
    pop @1
    pop @1


extendOrder
    :: Word32 & AccumPkDict & Hash SignMsgBody & Cell SignMsgBody & OrderDict & s
    :-> OrderDict & s
extendOrder = do
    push @2
    push @5
    dictGet
    if IsJust then do
        stacktype' @[Order, Word32]
        cast @Order @Slice
        decodeFromSliceFull @Order
        swap
        drop -- drop MsgBody from storage because there is one from msg
        moveOnTop @2
        swap
        false -- not new one
        roll @4
        --                                                    v whether new order or not
    else do
        swap
        newDict
        true -- new one
        roll @4

    swap
    cast @AccumPkDict @(DSet PublicKey)
    swap
    dictMerge

    if IsJust then do
        -- when not enough signatures
        stacktype' @[DSet PublicKey, Bool, Hash SignMsgBody, Cell SignMsgBody, OrderDict]
        roll @5
        roll @5
        moveOnTop @3
        moveOnTop @2
        stacktype' @[Cell SignMsgBody, DSet PublicKey, Hash SignMsgBody, OrderDict, Bool]
        dictEncodeSet
        swap
        ifElse addToTimestampSet ignore
    else do
        swap
        moveOnTop @3
        dictDelIgnore
        swap
        ifElse ignore removeFromTimestampSet
        swap
        decodeFromCell @SignMsgBody
        pushInt 0 -- msg type = 0
        sendRawMsg
        drop

-- TODO Add to set to perform garbage collection effectively
addToTimestampSet :: x & s :-> x & s
addToTimestampSet = ignore

-- TODO
removeFromTimestampSet :: x & s :-> x & s
removeFromTimestampSet = ignore
