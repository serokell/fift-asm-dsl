{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Types defined in this module are not used directly.
-- Only DecodeSlice/EncodeBuilder instances are valuable.
-- These types are only defined for clarity.

module MultiSig.Types
       ( SignMsg (..)
       , SignMsgBody (..)
       , SignPayload (..)
       , OrderIdPayload (..)
       , Storage (..)
       , Nonce (..)
       , OrderDict
       , OrderId
       , SignDict
       , MultiSigError (..)
       , TimestampDict
       , TimeNonce

       , timeNoncePack
       , unpackTime
       ) where

import Prelude

import FiftAsm

newtype Nonce = Nonce Word32
  deriving (Integral, Num, Real, Ord, Eq, Enum)

type instance ToTVM Nonce = 'IntT

instance DecodeSlice Nonce where
    decodeFromSliceImpl = ld32Unsigned
    preloadFromSliceImpl = pld32Unsigned

instance EncodeBuilder Nonce where
    encodeToBuilder = st32Unsigned

-- Msg part
type SignDict = Dict PublicKey Signature
data SignMsg = SignMsg
    { msgSignatures :: SignDict
    , msgBody       :: Cell SignMsgBody
    }

data SignMsgBody = SignMsgBody
    { mbNonce      :: Nonce
    , mbExpiration :: Timestamp
    , mbMsgObj     :: Cell MessageObject
    }

data SignPayload = SignPayload
    { spMsgBody :: Cell SignMsgBody
    , spMyAddr  :: Slice
    }

data OrderIdPayload = OrderIdPayload
    { oipExpiration :: Timestamp
    , oipMsgObj     :: Cell MessageObject
    }

instance EncodeBuilder OrderIdPayload where
    type EncodeBuilderFields OrderIdPayload = '[Timestamp, Cell MessageObject]
    encodeToBuilder = do
        encodeToBuilder @Timestamp
        encodeToBuilder @(Cell MessageObject)

instance EncodeBuilder SignPayload where
    type EncodeBuilderFields SignPayload = '[Slice, Cell SignMsgBody]
    encodeToBuilder = do
        stSlice
        encodeToBuilder @(Cell SignMsgBody)

instance DecodeSlice SignMsg where
    type DecodeSliceFields SignMsg = [Cell SignMsgBody, SignDict]
    decodeFromSliceImpl = do
        decodeFromSliceImpl @SignDict
        decodeFromSliceImpl @(Cell SignMsgBody)

instance DecodeSlice SignMsgBody where
    type DecodeSliceFields SignMsgBody = [Cell MessageObject, Timestamp, Nonce]
    decodeFromSliceImpl = do
        decodeFromSliceImpl @Nonce
        decodeFromSliceImpl @Timestamp
        decodeFromSliceImpl @(Cell MessageObject)

type OrderId = Hash OrderIdPayload

-- Storage part
type OrderDict = Dict OrderId (DSet PublicKey)
type TimestampDict = Dict TimeNonce OrderId

-- | Integer, containing nonce in greater 32 bits and time in smaller 32 bits
newtype TimeNonce = TimeNonce {unTimeNonce :: Word64}
  deriving (Eq, Ord, Show)
type instance ToTVM TimeNonce = 'IntT
type instance BitSize TimeNonce = 64
type instance IsUnsignedTF TimeNonce = 'True

unpackTime
  :: TimeNonce & s :-> Timestamp & s
unpackTime = do
  rshift 32
  cast @TimeNonce @Timestamp

timeNoncePack
  :: Timestamp & Nonce & s :-> TimeNonce & s
timeNoncePack = do
    cast @Timestamp @TimeNonce
    cast1 @Nonce @TimeNonce
    lshift 32
    add

data Storage = Storage
    { sOrders :: OrderDict
    , sNonce  :: Nonce
    , sK      :: Word32
    , sPKs    :: DSet PublicKey
    , sSorted :: TimestampDict
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

instance DecodeSlice TimeNonce where
    decodeFromSliceImpl = ld64Unsigned

instance EncodeBuilder TimeNonce where
    encodeToBuilder = st64Unsigned

data MultiSigError
    = NonceMismatch
    | MsgExpired
    | InvalidSignature
    | ErrorParsingMsg
    deriving (Eq, Ord, Show, Generic)

instance Exception MultiSigError

instance Enum MultiSigError where
    toEnum 32 = NonceMismatch
    toEnum 33 = MsgExpired
    toEnum 34 = InvalidSignature
    toEnum 35 = ErrorParsingMsg
    toEnum _ = error "Uknown MultiSigError id"

    fromEnum NonceMismatch = 32
    fromEnum MsgExpired = 33
    fromEnum InvalidSignature = 34
    fromEnum ErrorParsingMsg = 35
