{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Types defined in this module are not used directly.
-- Only DecodeSlice/EncodeBuilder instances are valuable.
-- These types are only defined for clarity.

module MultiSig.Types
       ( Msg (..)
       , MsgBody (..)
       , Storage (..)
       , Order (..)
       , Nonce (..)
       , OrderDict
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
