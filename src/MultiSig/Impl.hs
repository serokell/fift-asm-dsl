{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module MultiSig.Impl
       ( recvExternal
       ) where

import Prelude

import MultiSig.Types
import FiftAsm

recvExternal :: '[Slice] :-> '[]
recvExternal = do
    -- Garbage collection of expired orders
    pushRoot
    decodeFromCell @Storage
    garbageCollectOrders

    -- Load Msg on the stack
    moveOnTop @4
    decodeFromSliceFull @Msg
    stacktype @[Cell MsgBody, SignDict, Nonce, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Check that nonces of the storage and the message matched
    moveOnTop @2
    push @6
    compareNonces
    drop -- TODO
    stacktype @[Cell MsgBody, SignDict, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Check that the message hasn't expired
    dup
    checkMsgExpiration
    drop -- TODO implement check

    -- Compute the message body hash
    dup
    computeMsgBodyHash
    stacktype @[Hash MsgBody, Cell MsgBody, SignDict, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Remove signatures of the message which are not valid
    dup
    push @5
    swap
    moveOnTop @4
    filterValidSignatures
    stacktype @[DSet Signature, Hash MsgBody, Cell MsgBody, OrderDict, DSet PublicKey, Word32, Nonce]
    -- TODO check if empty

    -- Add valid signatures to the storage's OrderDict
    moveOnTop @3
    push @2
    mergeOrders
    stacktype @[OrderDict, Hash MsgBody, Cell MsgBody, DSet PublicKey, Word32, Nonce]

    -- Emit messages for orders which have at least K signatures
    push @4
    moveOnTop @3
    moveOnTop @3
    checkKSigs
    stacktype @[OrderDict, DSet PublicKey, Word32, Nonce]

    reversePrefix @4 -- reverse first 4 elements
    stacktype @[Nonce, Word32, DSet PublicKey, OrderDict]
    inc
    encodeToCell @Storage
    popRoot

garbageCollectOrders :: OrderDict & s :-> OrderDict & s
garbageCollectOrders = error "not implemented yet"

-- | Return true if nonces equal.
compareNonces :: Nonce & Nonce & s :-> Bool & s
compareNonces = equalInt

-- | Fetch expiraiton time from Cell MsgBody
getExpirationTime :: Cell MsgBody & s :-> Timestamp & s
getExpirationTime = do
    decodeFromCell @MsgBody
    drop

-- | Returns True, if a passed timestamp hasn't expired.
checkMsgExpiration :: Cell MsgBody & s :-> Bool & s
checkMsgExpiration = do
    getExpirationTime
    now
    greaterInt

computeMsgBodyHash:: Cell MsgBody & s :-> Hash MsgBody & s
computeMsgBodyHash = cellHash

filterValidSignatures :: SignDict & Hash MsgBody & DSet PublicKey & s :-> DSet Signature & s
filterValidSignatures = do
    newDict @Signature @()
    swap
    dictIter $ do
        stacktype' @[PublicKey, Signature, SignDict, DSet Signature, Hash MsgBody, DSet PublicKey]
        dup
        push @6
        dsetGet
        flip ifElse (drop >> drop) $ do
            stacktype' @[PublicKey, Signature, SignDict, DSet Signature, Hash MsgBody, DSet PublicKey]
            push @1
            swap
            push @5
            rollRev @3
            rollRev @3 -- TODO replace with roll
            chkSignU
            ifElse
                (moveOnTop @2 >> dsetSet >> swap)
                drop
    pop @1
    pop @1

mergeOrders :: Hash MsgBody & OrderDict & DSet Signature & s
            :-> OrderDict & s
mergeOrders = error "not implemented yet"

checkKSigs :: Hash MsgBody & Cell MsgBody  & Word32 & OrderDict & s :-> OrderDict & s
checkKSigs = error "not implemented"