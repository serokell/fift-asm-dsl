{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FiftAsm.Cell
       ( DecodeSlice (..)
       , decodeFromSliceFull
       , decodeCell

       , EncodeBuilder (..)
       , encodeCell
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

decodeCell :: forall a s . DecodeSlice a => Cell a & s :-> (DecodeSliceFields a ++ s)
decodeCell = do
    cToS @a
    decodeFromSliceFull @a

instance DecodeSlice Word32 where
    decodeFromSlice = I $ LDU 31

instance DecodeSlice (Dictionary k v) where
    decodeFromSlice = ldDict

instance DecodeSlice Signature where
    decodeFromSlice = I $ LDSLICE 511

instance DecodeSlice RawMsg where
    decodeFromSlice = do
        pushInt 0
        ldSliceX @Slice
        -- The first two instructions imitate creating of empty slice. dunno how to do it conventionally
        cast @Slice @RawMsg -- Then cast remaining Slice to RawMsg
        swap

instance DecodeSlice Timestamp where
    decodeFromSlice = I $ LDU 31

-- Encoding
class EncodeBuilder (a :: Kind.Type) where
    type EncodeBuilderFields a :: [Kind.Type]
    type instance EncodeBuilderFields a = Reverse (DecodeSliceFields a)
    encodeToBuilder :: Builder ': (EncodeBuilderFields a ++ s) :-> Builder & s

withBuilder :: forall a s t . (Builder & s :-> Builder & t) -> (s :-> Cell a & t)
withBuilder (I act) = I $ NEWC `Seq` act `Seq` ENDC

encodeCell :: forall a s . EncodeBuilder a => (EncodeBuilderFields a ++ s) :-> Cell a & s
encodeCell = withBuilder @a (encodeToBuilder @a)

instance EncodeBuilder Word32 where
    encodeToBuilder = I $ STU 31

instance EncodeBuilder (Dictionary k v) where
    encodeToBuilder = stDict

instance EncodeBuilder Signature where
    encodeToBuilder = stSlice

instance EncodeBuilder RawMsg where
    encodeToBuilder = stSlice

instance EncodeBuilder Timestamp where
    encodeToBuilder = I $ STU 31

-- TODO: define instances for Generic and Vinyl record
-- TODO: define constraints which would be check that instances are defined
--       to avoid misleading type errors