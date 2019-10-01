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
import Data.Vinyl.TypeLevel (type (++))

import FiftAsm

newtype Nonce = Nonce Word32

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
data Storage = Storage
    { sNonce  :: Nonce
    , sK      :: Word32
    , sPKs    :: DSet PublicKey
    , sOrders :: Dictionary (Hash MsgBody) Order
    }

data Order = Order
    { oMsgBody    :: MsgBody
    , oSignatures :: DSet Signature
    }

instance DecodeSlice Storage where
    type DecodeSliceFields Storage = [Dictionary (Hash MsgBody) Order, DSet PublicKey, Word32, Nonce]
    decodeSlice = do
        decodeSlice @Nonce
        decodeSlice @Word32
        decodeSlice @(DSet PublicKey)
        decodeSlice @(Dictionary (Hash MsgBody) Order)

instance DecodeSlice Order where
    type DecodeSliceFields Order = DSet Signature ': DecodeSliceFields MsgBody
    decodeSlice = do
        decodeSlice @MsgBody
        decodeSlice @(DSet Signature)

recvExternal :: '[Slice] :-> '[]
recvExternal = do
    decodeSliceFull @Msg

    drop
    drop
    drop
    drop
