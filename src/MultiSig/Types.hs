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
       , decodeMsgFromSliceFull
       ) where

import Prelude

import FiftAsm

newtype Nonce = Nonce Word32

type instance ToTVM Nonce = 'IntT

instance DecodeSlice Nonce where
    decodeFromSlice = ld32Unsigned

instance EncodeBuilder Nonce where
    encodeToBuilder = st32Unsigned

-- data Msg = GetAllOrders | GetOrdersByKey PublicKey | SignMsg_ SignMsg

decodeMsgFromSliceFull
  :: '[] :-> '[]
  -> '[PublicKey] :-> '[]
  -> DecodeSliceFields SignMsg :-> '[]
  -> '[Slice] :-> '[]
decodeMsgFromSliceFull handleGetAll handleGetByKey handleSignMsg = do
  decodeFromSlice @Word32
  swap
  dup
  pushInt 2
  if IsEq
    then drop >> endS >> handleGetAll
    else do
      dup
      pushInt 1
      if IsEq
        then drop >> decodeFromSlice @PublicKey >> endS >> handleGetByKey
        else do
          pushInt 0
          if IsEq
            then decodeFromSlice @SignMsg >> endS >> handleSignMsg
            else throw ErrorParsingMsg

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
    decodeFromSlice = do
        decodeFromSlice @Nonce
        decodeFromSlice @SignDict
        decodeFromSlice @(Cell SignMsgBody)

instance DecodeSlice SignMsgBody where
    type DecodeSliceFields SignMsgBody = [Cell MessageObject, Timestamp]
    decodeFromSlice = do
        decodeFromSlice @Timestamp
        decodeFromSlice @(Cell MessageObject)

-- Storage part
type OrderDict =  Dict (Hash SignMsgBody) Order
data Storage = Storage
    { sOrders :: OrderDict
    , sNonce  :: Nonce
    , sK      :: Word32
    , sPKs    :: DSet PublicKey
    }

data Order = Order
    { oMsgBody    :: Cell SignMsgBody
    , oSignatures :: Dict PublicKey Signature
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
    type DecodeSliceFields Order = [Dict PublicKey Signature, Cell SignMsgBody]
    decodeFromSlice = do
        decodeFromSlice @(Cell SignMsgBody)
        decodeFromSlice @(Dict PublicKey Signature)

instance EncodeBuilder Order where
    encodeToBuilder = do
        encodeToBuilder @(Cell SignMsgBody)
        encodeToBuilder @(Dict PublicKey Signature)

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
