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
recvGetAllOrders = do
    -- Garbage collection of expired orders
    pushRoot
    comment "Decoding of storage fields"
    decodeFromCell @Storage
    roll @4
    drop >> drop >> drop
    stacktype' @'[OrderDict]
    mkMethodReturnMessage
    sendRawMsg

mkMethodReturnMessage
  :: '[OrderDict]
  :-> '[Word32, Cell MessageObject]
mkMethodReturnMessage = undefined

recvGetOrdersByKey :: '[PublicKey] :-> '[]
recvGetOrdersByKey = undefined

recvSignMsg :: DecodeSliceFields SignMsg :-> '[]
recvSignMsg = do
    -- Garbage collection of expired orders
    pushRoot
    comment "Decoding of storage fields"
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
    stacktype @[DSet Signature, Hash SignMsgBody, Cell SignMsgBody, OrderDict, DSet PublicKey, Word32, Nonce]
    dup
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

filterValidSignatures :: SignDict & Hash SignMsgBody & DSet PublicKey & s :-> DSet Signature & s
filterValidSignatures = do
    newDict @Signature @()
    swap
    dictIter $ do
        stacktype' @[PublicKey, Signature, SignDict, DSet Signature, Hash SignMsgBody, DSet PublicKey]
        dup
        push @6
        dsetGet
        if NotHolds then
            drop >> drop
        else do
            stacktype' @[PublicKey, Signature, SignDict, DSet Signature, Hash SignMsgBody, DSet PublicKey]
            push @1
            swap
            push @5
            roll @3
            chkSignU
            if Holds then do
                moveOnTop @2
                dsetSet
                swap
            else
                drop
    pop @1
    pop @1


extendOrder
    :: Word32 & DSet Signature & Hash SignMsgBody & Cell SignMsgBody & OrderDict & s
    :-> OrderDict & s
extendOrder = do
    push @2
    push @5
    dictGet
    if IsJust then do
        stacktype' @[Order, Word32, DSet Signature]
        cast @Order @Slice
        decodeFromSliceFull @Order
        swap
        drop -- drop MsgBody from storage because there is one from msg
        moveOnTop @2
        swap
        false -- not new one
        roll @4
        --                                                    v whether new order or not
        stacktype' @[DSet Signature, DSet Signature, Word32, Bool]
    else do
        swap
        newDict @Signature @()
        true -- new one
        roll @4

    dictMerge

    if IsJust then do
        -- when not enough signatures
        stacktype' @[DSet Signature, Bool, Hash SignMsgBody, Cell SignMsgBody, OrderDict]
        rollRev @4
        encodeToSlice @Order
        cast @Slice @Order
        moveOnTop @2
        moveOnTop @3
        dictSet @(Hash SignMsgBody) @Order
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
