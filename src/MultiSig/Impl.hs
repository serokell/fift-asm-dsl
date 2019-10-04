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

    -- Check that nonces of the storage and the message matched
    moveOnTop @4
    decodeFromSliceFull @Msg
    moveOnTop @3
    push @7
    compareNonces
    drop -- TODO
    stacktype @[RawMsg, Timestamp, SignDict, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Check that the message hasn't expired
    push @1
    checkMsgExpiration
    drop -- TODO

    -- Compute the message body hash
    dup
    push @2
    computeMsgBodyHash
    pop @3
    stacktype @[Hash MsgBody, Slice, RawMsg, SignDict, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Remove signatures of the message which are not valid
    push @5
    moveOnTop @2
    moveOnTop @4
    filterValidSignatures
    stacktype @[DSet Signature, Hash MsgBody, RawMsg, OrderDict, DSet PublicKey, Word32, Nonce]
    -- TODO

    -- Add valid signatures to the storage's OrderDict
    moveOnTop @3
    push @2
    mergeOrders
    stacktype @[OrderDict, Hash MsgBody, RawMsg, DSet PublicKey, Word32, Nonce]

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

-- | Returns True, if a passed timestamp hasn't expired.
checkMsgExpiration :: Timestamp & s :-> Bool & s
checkMsgExpiration = do
    now
    greaterInt

computeMsgBodyHash:: Timestamp & RawMsg & s :-> Hash MsgBody & Slice & s
computeMsgBodyHash = do
    encodeToSlice @MsgBody
    dup
    dataHash @MsgBody

filterValidSignatures :: SignDict & Slice & DSet PublicKey & s :-> DSet Signature & s
filterValidSignatures = do
    newDict @Signature @()
    swap
    dictIter $ do
        stacktype' @[PublicKey, Signature, SignDict, DSet Signature, Slice, DSet PublicKey]
        dup
        push @6
        dsetGet
        flip ifElse (drop >> drop) $ do
            stacktype' @[PublicKey, Signature, SignDict, DSet Signature, Slice, DSet PublicKey]
            push @1
            swap
            push @5
            rollRev @3
            rollRev @3 -- TODO replace with roll
            checkSignS
            ifElse
                (moveOnTop @2 >> dsetSet >> swap)
                drop
    pop @1
    pop @1

mergeOrders :: Hash MsgBody & OrderDict & DSet Signature & s
            :-> OrderDict & s
mergeOrders = error "not implemented yet"

checkKSigs :: Hash MsgBody & RawMsg  & Word32 & OrderDict & s :-> OrderDict & s
checkKSigs = error "not implemented"