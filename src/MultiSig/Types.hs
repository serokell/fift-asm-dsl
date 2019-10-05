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
       , SignDict
       ) where

import Prelude

import FiftAsm

newtype Nonce = Nonce Word32

type instance ToTVM Nonce = 'IntT

instance DecodeSlice Nonce where
    decodeFromSlice = ld32Unsigned

instance EncodeBuilder Nonce where
    encodeToBuilder = st32Unsigned

-- Msg part
type SignDict = Dict PublicKey Signature
data Msg = Msg
    { msgNonce      :: Nonce
    , msgSignatures :: SignDict
    , msgBody       :: Cell MsgBody
    }

data MsgBody = MsgBody
    { mbExpiration :: Timestamp
    , mbMsgObj     :: Cell MessageObject
    }

instance DecodeSlice Msg where
    type DecodeSliceFields Msg = [Cell MsgBody, SignDict, Nonce]
    decodeFromSlice = do
        decodeFromSlice @Nonce
        decodeFromSlice @SignDict
        decodeFromSlice @(Cell MsgBody)

instance DecodeSlice MsgBody where
    type DecodeSliceFields MsgBody = [Cell MessageObject, Timestamp]
    decodeFromSlice = do
        decodeFromSlice @Timestamp
        decodeFromSlice @(Cell MessageObject)

-- Storage part
type OrderDict =  Dict (Hash MsgBody) Order
data Storage = Storage
    { sNonce  :: Nonce
    , sK      :: Word32
    , sPKs    :: DSet PublicKey
    , sOrders :: OrderDict
    }

data Order = Order
    { oMsgBody    :: Cell MsgBody
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
    type DecodeSliceFields Order = [DSet Signature, Cell MsgBody]
    decodeFromSlice = do
        decodeFromSlice @(Cell MsgBody)
        decodeFromSlice @(DSet Signature)

instance EncodeBuilder Order where
    encodeToBuilder = do
        encodeToBuilder @(Cell MsgBody)
        encodeToBuilder @(DSet Signature)

type instance ToTVM Order = 'SliceT