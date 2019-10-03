{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module MultiSig
       ( recvExternal
       , Msg (..)
       , MsgBody (..)
       , Storage (..)
       , Order (..)
       , Nonce (..)
       ) where

import Prelude

import FiftAsm
import Util

newtype Nonce = Nonce Word32

type instance ToTVM Nonce = 'IntT

instance DecodeSlice Nonce where
    decodeFromSlice = ld32Unsigned

instance EncodeBuilder Nonce where
    encodeToBuilder = st32Unsigned

-- Msg part
data Msg = Msg
    { msgNonce      :: Nonce
    , msgSignatures :: DSet Signature
    , msgAddress    :: MsgBody
    }

data MsgBody = MsgBody
    { mbExpiration :: Timestamp
    , mbRawMsg     :: RawMsg
    }

instance DecodeSlice Msg where
    type DecodeSliceFields Msg = DecodeSliceFields MsgBody ++ [DSet Signature, Nonce]
    decodeFromSlice = do
        decodeFromSlice @Nonce
        decodeFromSlice @(DSet Signature)
        decodeFromSlice @MsgBody

instance DecodeSlice MsgBody where
    type DecodeSliceFields MsgBody = [RawMsg, Timestamp]
    decodeFromSlice = do
        decodeFromSlice @Timestamp
        decodeFromSlice @RawMsg

instance EncodeBuilder MsgBody where
    encodeToBuilder = do
        encodeToBuilder @Timestamp
        encodeToBuilder @RawMsg

-- Storage part
type OrderDict =  Dictionary (Hash MsgBody) Order
data Storage = Storage
    { sNonce  :: Nonce
    , sK      :: Word32
    , sPKs    :: DSet PublicKey
    , sOrders :: OrderDict
    }

data Order = Order
    { oMsgBody    :: MsgBody
    , oSignatures :: DSet Signature
    }

instance DecodeSlice Storage where
    type DecodeSliceFields Storage = [OrderDict, DSet PublicKey, Word32, Nonce]
    decodeFromSlice = do
        decodeFromSlice @Nonce
        decodeFromSlice @Word32
        decodeFromSlice @(DSet PublicKey)
        decodeFromSlice @OrderDict

instance EncodeBuilder Storage where
    encodeToBuilder = do
        encodeToBuilder @Nonce
        encodeToBuilder @Word32
        encodeToBuilder @(DSet PublicKey)
        encodeToBuilder @OrderDict

instance DecodeSlice Order where
    type DecodeSliceFields Order = DSet Signature ': DecodeSliceFields MsgBody
    decodeFromSlice = do
        decodeFromSlice @MsgBody
        decodeFromSlice @(DSet Signature)

recvExternal :: '[Slice] :-> '[]
recvExternal = do
    -- Garbage collection of expired orders
    pushRoot
    decodeCell @Storage
    garbageCollectOrders

    -- Check that nonces of the storage and the message matched
    move @4
    decodeFromSliceFull @Msg
    move @3
    push @7
    compareNonces
    drop -- TODO
    stacktype @[RawMsg, Timestamp, DSet Signature, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Check that the message hasn't expired
    push @1
    checkMsgExpiration
    drop -- TODO

    -- Compute the message body hash
    push @0
    push @2
    computeMsgBodyHash
    pop @2
    -- TODO
    stacktype @[Hash MsgBody, RawMsg, DSet Signature, OrderDict, DSet PublicKey, Word32, Nonce]

    -- Remove signatures of the message which are not valid
    move @2
    filterValidSignatures
    stacktype @[DSet Signature, Hash MsgBody, RawMsg, OrderDict, DSet PublicKey, Word32, Nonce]
    -- TODO

    -- Add valid signatures to the storage's OrderDict
    move @3
    push @2
    mergeOrders
    stacktype @[OrderDict, Hash MsgBody, RawMsg, DSet PublicKey, Word32, Nonce]

    -- Emit messages for orders which have at least K signatures
    push @4
    move @3
    move @3
    checkKSigs
    stacktype @[OrderDict, DSet PublicKey, Word32, Nonce]

    reversePrefix @2 -- reverse first 4 elements
    stacktype @[Nonce, Word32, DSet PublicKey, OrderDict]
    encodeCell @Storage
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

computeMsgBodyHash:: Timestamp & RawMsg & s :-> Hash MsgBody & s
computeMsgBodyHash = do
    encodeCell @MsgBody
    cellHash

filterValidSignatures :: DSet Signature & s :-> DSet Signature & s
filterValidSignatures = error "not implemented yet"

mergeOrders :: Hash MsgBody & OrderDict & DSet Signature & s
            :-> OrderDict & s
mergeOrders = error "not implemented yet"

checkKSigs :: Hash MsgBody & RawMsg  & Word32 & OrderDict & s :-> OrderDict & s
checkKSigs = error "not implemented"