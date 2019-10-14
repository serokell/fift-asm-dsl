{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FiftAsm.Cell
       ( DecodeSlice (..)
       , decodeFromSlice
       , decodeFromSliceFull
       , decodeFromCell
       , decodeFromSliceUnsafe
       , preloadFromSlice
       , preloadFromCell

       , EncodeBuilder (..)
       , encodeToCell
       , encodeToSlice
       ) where

import Prelude
import Data.Vinyl.TypeLevel (type (++))
import qualified Data.Kind as Kind
import Data.Typeable (Typeable, typeRep)

import FiftAsm.DSL
import FiftAsm.Instr
import Util

-- Decoding
class DecodeSlice (a :: Kind.Type) where
    type DecodeSliceFields a :: [Kind.Type]
    type instance DecodeSliceFields a = '[a]
    decodeFromSliceImpl :: (Slice & s) :-> (Slice & (DecodeSliceFields a ++ s))

    preloadFromSliceImpl :: (Slice & s) :-> (DecodeSliceFields a ++ s)
    preloadFromSliceImpl = decodeFromSliceImpl @a >> drop

decodeFromSlice
    :: forall a s .
      ( DecodeSlice a
      , Typeable a
      , Typeable (DecodeSliceFields a)
      , (DecodeSliceFields a ++ '[]) ~ DecodeSliceFields a
      )
    => (Slice & s) :-> (Slice & (DecodeSliceFields a ++ s))
decodeFromSlice = do
    comment $ "Decode from slice @" <> show (typeRep (Proxy @a))
    viaSubroutine @'[Slice]
                  @(Slice & DecodeSliceFields a) "decodeFromSlice" $ do
      comment $ "Decode from slice @" <> show (typeRep (Proxy @a))
      decodeFromSliceImpl @a


decodeFromSliceFull
    :: forall a s .
      ( DecodeSlice a
      , Typeable a
      , Typeable (DecodeSliceFields a)
      , (DecodeSliceFields a ++ '[]) ~ DecodeSliceFields a
      )
    => Slice & s :-> (DecodeSliceFields a ++ s)
decodeFromSliceFull = do
    decodeFromSlice @a
    endS

decodeFromCell
    :: forall a s .
      ( DecodeSlice a
      , Typeable a
      , Typeable (DecodeSliceFields a)
      , (DecodeSliceFields a ++ '[]) ~ DecodeSliceFields a
      )
    => Cell a & s :-> (DecodeSliceFields a ++ s)
decodeFromCell = do
    cToS @a
    decodeFromSliceFull @a

-- This version doesn't check that Slice is fully consumed
decodeFromSliceUnsafe
    :: forall a s .
      ( DecodeSlice a
      , Typeable a
      , Typeable (DecodeSliceFields a)
      , (DecodeSliceFields a ++ '[]) ~ DecodeSliceFields a
      )
    => Slice & s :-> (DecodeSliceFields a ++ s)
decodeFromSliceUnsafe = decodeFromSlice @a >> drop



preloadFromSlice
    :: forall a s .
      ( DecodeSlice a
      , Typeable a
      , Typeable (DecodeSliceFields a)
      , (DecodeSliceFields a ++ '[]) ~ DecodeSliceFields a
      )
    => (Slice & s) :-> (DecodeSliceFields a ++ s)
preloadFromSlice = do
    comment $ "Preload from slice @" <> show (typeRep (Proxy @a))
    viaSubroutine @'[Slice] @(DecodeSliceFields a) "decodeFromSlice" $ do
      comment $ "Preload from slice @" <> show (typeRep (Proxy @a))
      preloadFromSliceImpl @a

preloadFromCell
    :: forall a s .
      ( DecodeSlice a
      , Typeable a
      , Typeable (DecodeSliceFields a)
      , (DecodeSliceFields a ++ '[]) ~ DecodeSliceFields a
      )
    => Cell a & s :-> (DecodeSliceFields a ++ s)
preloadFromCell = do
    cToS @a
    preloadFromSlice @a


instance DecodeSlice Word32 where
    decodeFromSliceImpl = mkI $ LDU 32
    preloadFromSliceImpl = mkI $ PLDU 32

instance DecodeSlice Signature where
    decodeFromSliceImpl = pushInt @Integer 512 >> mkI LDSLICEX
    preloadFromSliceImpl = pushInt @Integer 512 >> mkI PLDSLICEX

instance DecodeSlice PublicKey where
    decodeFromSliceImpl = mkI $ LDU 256
    preloadFromSliceImpl = mkI $ PLDU 256

instance DecodeSlice (Hash a) where
    decodeFromSliceImpl = mkI $ LDU 256
    preloadFromSliceImpl = mkI $ PLDU 256

instance DecodeSlice (Cell a) where
    decodeFromSliceImpl = mkI LDREF
    preloadFromSliceImpl = mkI PLDREF

instance DecodeSlice Timestamp where
    decodeFromSliceImpl = mkI $ LDU 32
    preloadFromSliceImpl = mkI $ PLDU 32

-- Mock for DSet
instance DecodeSlice () where
    decodeFromSliceImpl = unit >> swap
    preloadFromSliceImpl = unit >> nip

-- Encoding
class EncodeBuilder (a :: Kind.Type) where
    type EncodeBuilderFields a :: [Kind.Type]
    type instance EncodeBuilderFields a = Reverse (DecodeSliceFields a)
    encodeToBuilder :: Builder ': (EncodeBuilderFields a ++ s) :-> Builder & s

withBuilder :: forall a s t . (Builder & s :-> Builder & t) -> (s :-> Cell a & t)
withBuilder (I act r) = I (NEWC `Seq` act `Seq` ENDC) r

encodeToCell :: forall a s . EncodeBuilder a => (EncodeBuilderFields a ++ s) :-> Cell a & s
encodeToCell = withBuilder @a (encodeToBuilder @a)

encodeToSlice :: forall a s . EncodeBuilder a => (EncodeBuilderFields a ++ s) :-> Slice & s
encodeToSlice = encodeToCell @a >> cToS @a

instance EncodeBuilder Word32 where
    encodeToBuilder = mkI $ STU 32

instance EncodeBuilder Signature where
    encodeToBuilder = stSlice

instance EncodeBuilder PublicKey where
    encodeToBuilder = mkI $ STU 256

instance EncodeBuilder (Hash a) where
    encodeToBuilder = mkI $ STU 256

instance EncodeBuilder (Cell a) where
    encodeToBuilder = mkI STREF

instance EncodeBuilder Timestamp where
    encodeToBuilder = mkI $ STU 32

-- Mock for DSet
instance EncodeBuilder () where
    encodeToBuilder = swap >> drop

-- TODO: define instances for Generic and Vinyl record
-- TODO: define constraints which would be check that instances are defined
--       to avoid misleading type errors
