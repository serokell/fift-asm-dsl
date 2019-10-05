{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FiftAsm.Cell
       ( DecodeSlice (..)
       , decodeFromSliceFull
       , decodeFromCell
       , decodeFromSliceUnsafe

       , EncodeBuilder (..)
       , encodeToCell
       , encodeToSlice
       ) where

import Prelude
import Data.Vinyl.TypeLevel (type (++))
import qualified Data.Kind as Kind

import FiftAsm.DSL
import FiftAsm.Instr
import Util

-- Decoding
class DecodeSlice (a :: Kind.Type) where
    type DecodeSliceFields a :: [Kind.Type]
    type instance DecodeSliceFields a = '[a]
    decodeFromSlice :: (Slice & s) :-> (Slice & (DecodeSliceFields a ++ s))

decodeFromSliceFull :: forall a s . DecodeSlice a => Slice & s :-> (DecodeSliceFields a ++ s)
decodeFromSliceFull = do
    decodeFromSlice @a
    endS

decodeFromCell :: forall a s . DecodeSlice a => Cell a & s :-> (DecodeSliceFields a ++ s)
decodeFromCell = do
    cToS @a
    decodeFromSliceFull @a

-- This version doesn't check that Slice is fully consumed
decodeFromSliceUnsafe :: forall a s . DecodeSlice a => Slice & s :-> (DecodeSliceFields a ++ s)
decodeFromSliceUnsafe = decodeFromSlice @a >> drop

instance DecodeSlice Word32 where
    decodeFromSlice = I $ LDU 32

instance DecodeSlice Signature where
    decodeFromSlice = I $ LDSLICE 512

instance DecodeSlice PublicKey where
    decodeFromSlice = I $ LDU 256

instance DecodeSlice (Hash a) where
    decodeFromSlice = I $ LDU 256

instance DecodeSlice (Cell a) where
    decodeFromSlice = I LDREF

instance DecodeSlice Timestamp where
    decodeFromSlice = I $ LDU 32

-- Mock for DSet
instance DecodeSlice () where
    decodeFromSlice = unit >> swap

-- Encoding
class EncodeBuilder (a :: Kind.Type) where
    type EncodeBuilderFields a :: [Kind.Type]
    type instance EncodeBuilderFields a = Reverse (DecodeSliceFields a)
    encodeToBuilder :: Builder ': (EncodeBuilderFields a ++ s) :-> Builder & s

withBuilder :: forall a s t . (Builder & s :-> Builder & t) -> (s :-> Cell a & t)
withBuilder (I act) = I $ NEWC `Seq` act `Seq` ENDC

encodeToCell :: forall a s . EncodeBuilder a => (EncodeBuilderFields a ++ s) :-> Cell a & s
encodeToCell = withBuilder @a (encodeToBuilder @a)

encodeToSlice :: forall a s . EncodeBuilder a => (EncodeBuilderFields a ++ s) :-> Slice & s
encodeToSlice = encodeToCell @a >> cToS @a

instance EncodeBuilder Word32 where
    encodeToBuilder = I $ STU 32

instance EncodeBuilder Signature where
    encodeToBuilder = stSlice

instance EncodeBuilder PublicKey where
    encodeToBuilder = I $ STU 256

instance EncodeBuilder (Hash a) where
    encodeToBuilder = I $ STU 256

instance EncodeBuilder (Cell a) where
    encodeToBuilder = I STREF

instance EncodeBuilder Timestamp where
    encodeToBuilder = I $ STU 32

-- Mock for DSet
instance EncodeBuilder () where
    encodeToBuilder = swap >> drop

-- TODO: define instances for Generic and Vinyl record
-- TODO: define constraints which would be check that instances are defined
--       to avoid misleading type errors