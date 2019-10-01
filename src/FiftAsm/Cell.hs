{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FiftAsm.Cell
       ( DecodeSlice (..)
       , decodeSliceFull
       ) where

import Prelude
import Data.Vinyl.TypeLevel (type (++))
import qualified Data.Kind as Kind

import FiftAsm.DSL
import FiftAsm.Instr

class DecodeSlice (a :: Kind.Type) where
    type DecodeSliceFields a :: [Kind.Type]
    type instance DecodeSliceFields a = '[a]
    decodeSlice :: (Slice & s) :-> (Slice & (DecodeSliceFields a ++ s))

decodeSliceFull :: forall a s . DecodeSlice a => Slice & s :-> (DecodeSliceFields a ++ s)
decodeSliceFull = do
    decodeSlice @a
    endS

instance DecodeSlice Word32 where
    decodeSlice = I $ LDU 31

instance DecodeSlice (Dictionary k v) where
    decodeSlice = ldDict

instance DecodeSlice Signature where
    decodeSlice = I $ LDSLICE 511

instance DecodeSlice RawMsg where
    decodeSlice = do
        pushInt 0
        ldSliceX @Slice
        -- The first two instructions imitate creating of empty slice. dunno how to do it conventionally
        cast @Slice @RawMsg -- Then cast remaining Slice to RawMsg
        swap

instance DecodeSlice Timestamp where
    decodeSlice = I $ LDU 31

-- TODO: define instances for Generic and Vinyl record