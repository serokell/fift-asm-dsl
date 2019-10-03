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
    decodeSlice = I (LDU 31 `Seq` Ignore)

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
    decodeSlice = do
        decodeSlice @Nonce
        decodeSlice @(DSet Signature)
        decodeSlice @MsgBody

instance DecodeSlice MsgBody where
    type DecodeSliceFields MsgBody = [RawMsg, Timestamp]
    decodeSlice = do
        decodeSlice @Timestamp
        decodeSlice @RawMsg

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
    decodeSlice = do
        decodeSlice @Nonce
        decodeSlice @Word32
        decodeSlice @(DSet PublicKey)
        decodeSlice @OrderDict

instance DecodeSlice Order where
    type DecodeSliceFields Order = DSet Signature ': DecodeSliceFields MsgBody
    decodeSlice = do
        decodeSlice @MsgBody
        decodeSlice @(DSet Signature)

recvExternal :: '[Slice] :-> '[]
recvExternal = do
    pushRoot
    decodeSliceFull @Storage
    garbageCollectOrders

    move @4
    decodeSliceFull @Msg
    move @3
    push @7
    compareNonces
    drop -- TODO
    stacktype @[RawMsg, Timestamp, DSet Signature, OrderDict, DSet PublicKey, Word32, Nonce]

    push @1
    checkMsgExpiration
    drop -- TODO

    push @0
    push @2
    computeMsgBodyHash
    pop @2
    -- TODO
    stacktype @[Hash MsgBody, RawMsg, DSet Signature, OrderDict, DSet PublicKey, Word32, Nonce]

    move @2
    filterValidSignatures
    stacktype @[DSet Signature, Hash MsgBody, RawMsg, OrderDict, DSet PublicKey, Word32, Nonce]
    -- TODO

    move @3
    push @2
    mergeOrders
    stacktype @[OrderDict, Hash MsgBody, RawMsg, DSet PublicKey, Word32, Nonce]

    push @4
    move @3
    move @3
    checkKSigs

    drop >> drop >> drop >> drop

garbageCollectOrders :: OrderDict & s :-> OrderDict & s
garbageCollectOrders = error "not implemented yet"

compareNonces :: Nonce & Nonce & s :-> Bool & s
compareNonces = error "not implemented"

checkMsgExpiration :: Timestamp & s :-> Bool & s
checkMsgExpiration = error "not implemented yet"

computeMsgBodyHash:: Timestamp & RawMsg & s :-> Hash MsgBody & s
computeMsgBodyHash = error "not implemented yet"

filterValidSignatures :: DSet Signature & s :-> DSet Signature & s
filterValidSignatures = error "not implemented yet"

mergeOrders :: Hash MsgBody & OrderDict & DSet Signature & s
            :-> OrderDict & s
mergeOrders = error "not implemented yet"

checkKSigs :: Hash MsgBody & RawMsg  & Word32 & OrderDict & s :-> OrderDict & s
checkKSigs = error "not implemented"