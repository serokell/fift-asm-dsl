{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Types defined in this module are not used directly.
-- Only DecodeSlice/EncodeBuilder instances are valuable.
-- These types are only defined for clarity.

module MultiSig.Types
       ( SignMsg (..)
       , SignMsgBody (..)
       , Storage (..)
       , Order (..)
       , Nonce (..)
       , OrderDict
       , SignDict
       , MultiSigError (..)
       , TimestampDict
       , TimeNonce
       ) where

import Prelude

import FiftAsm

newtype Nonce = Nonce Word32

type instance ToTVM Nonce = 'IntT

instance DecodeSlice Nonce where
    decodeFromSliceImpl = ld32Unsigned
    preloadFromSliceImpl = pld32Unsigned

instance EncodeBuilder Nonce where
    encodeToBuilder = st32Unsigned


-- Msg part
type SignDict = Dict PublicKey Signature
data SignMsg = SignMsg
    { msgNonce      :: Nonce
    , msgSignatures :: SignDict
    , msgBody       :: Cell SignMsgBody
    }

data SignMsgBody = SignMsgBody
    { mbExpiration :: Timestamp
    , mbMsgObj     :: Cell MessageObject
    }

instance DecodeSlice SignMsg where
    type DecodeSliceFields SignMsg = [Cell SignMsgBody, SignDict, Nonce]
    decodeFromSliceImpl = do
        decodeFromSliceImpl @Nonce
        decodeFromSliceImpl @SignDict
        decodeFromSliceImpl @(Cell SignMsgBody)

instance DecodeSlice SignMsgBody where
    type DecodeSliceFields SignMsgBody = [Cell MessageObject, Timestamp]
    decodeFromSliceImpl = do
        decodeFromSliceImpl @Timestamp
        decodeFromSliceImpl @(Cell MessageObject)

-- Storage part
type OrderDict =  Dict (Hash SignMsgBody) Order
type TimestampDict = Dict TimeNonce (Hash SignMsgBody)
newtype TimeNonce = TimeNonce {unTimeNonce :: Word64}
  deriving (Eq, Ord, Show)
type instance ToTVM TimeNonce = 'IntT
type instance BitSize TimeNonce = 64
type instance IsUnsignedTF TimeNonce = 'True

data Storage = Storage
    { sOrders :: OrderDict
    , sNonce  :: Nonce
    , sK      :: Word32
    , sPKs    :: DSet PublicKey
    , sSorted :: TimestampDict
    }

data Order = Order
    { oMsgBody    :: Cell SignMsgBody
    , oApproved   :: DSet PublicKey
    }

instance DecodeSlice Storage where
    type DecodeSliceFields Storage = [TimestampDict, OrderDict, DSet PublicKey, Word32, Nonce]
    decodeFromSliceImpl = do
        decodeFromSliceImpl @Nonce
        decodeFromSliceImpl @Word32
        decodeFromSliceImpl @(DSet PublicKey)
        decodeFromSliceImpl @OrderDict
        decodeFromSliceImpl @TimestampDict

instance EncodeBuilder Storage where
    encodeToBuilder = do
        encodeToBuilder @Nonce
        encodeToBuilder @Word32
        encodeToBuilder @(DSet PublicKey)
        encodeToBuilder @OrderDict
        encodeToBuilder @TimestampDict

instance DecodeSlice Order where
    type DecodeSliceFields Order = [DSet PublicKey, Cell SignMsgBody]
    decodeFromSliceImpl = do
        decodeFromSliceImpl @(Cell SignMsgBody)
        decodeFromSliceImpl @(DSet PublicKey)

instance EncodeBuilder Order where
    encodeToBuilder = do
        encodeToBuilder @(Cell SignMsgBody)
        encodeToBuilder @(DSet PublicKey)

instance DecodeSlice TimeNonce where
    decodeFromSliceImpl = ld64Unsigned

instance EncodeBuilder TimeNonce where
    encodeToBuilder = st64Unsigned

type instance ToTVM Order = 'SliceT

data MultiSigError
    = NonceMismatch
    | MsgExpired
    | NoValidSignatures
    | ErrorParsingMsg
    deriving (Eq, Ord, Show, Generic)

instance Exception MultiSigError

instance Enum MultiSigError where
    toEnum 32 = NonceMismatch
    toEnum 33 = MsgExpired
    toEnum 34 = NoValidSignatures
    toEnum 35 = ErrorParsingMsg
    toEnum _ = error "Uknown MultiSigError id"

    fromEnum NonceMismatch = 32
    fromEnum MsgExpired = 33
    fromEnum NoValidSignatures = 34
    fromEnum ErrorParsingMsg = 35
