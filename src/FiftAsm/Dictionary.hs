{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PolyKinds #-}

module FiftAsm.Dictionary
       ( Dict
       , DSet
       , newDict
       , ldDict
       , stDict
       , dsetGet
       , dictGet
       , dictRemMin
       , dictIter
       , dictSet
       , dsetSet
       ) where

import Prelude

import FiftAsm.Instr
import FiftAsm.Types
import FiftAsm.DSL
import FiftAsm.Cell

data Dict k v
type DSet k = Dict k ()
type instance ToTVM (Dict k v) = 'DictT

instance DecodeSlice (Dict k v) where
    decodeFromSlice = ldDict

instance EncodeBuilder (Dict k v) where
    encodeToBuilder = stDict

-- Getter / Setter primitives

class DictGetter k v where
    dictGet :: Dict k v & k & s :-> Mb '[v] & s

class DictSetter k v where
    dictSet :: Dict k v & k & v & s :-> Dict k v & s

class (ToTVM k ~ tvmk, ToTVM v ~ tvmv, IsUnsignedTF k ~ isUnsignedFlag)
      => DictGetter' tvmk tvmv isUnsignedFlag k v where
    dictGet' :: Dict k v & k & s :-> Mb '[v] & s

instance (ToTVM k ~ 'SliceT, ToTVM v ~ 'SliceT, IsUnsignedTF k ~ 'False, KnownNat (BitSize k))
        => DictGetter' 'SliceT 'SliceT 'False k v where
    dictGet' = do
        pushInt (bitSize @k)
        I DICTGET

instance (ToTVM k ~ 'IntT, ToTVM v ~ 'SliceT, IsUnsignedTF k ~ 'True, KnownNat (BitSize k))
        => DictGetter' 'IntT 'SliceT 'True k v where
    dictGet' = do
        pushInt (bitSize @k)
        I DICTUGET

instance DictGetter' (ToTVM k) (ToTVM v) (IsUnsignedTF k) k v => DictGetter k v where
    dictGet = dictGet'

-- Dict Setter

class (ToTVM k ~ tvmk, ToTVM v ~ tvmv, IsUnsignedTF k ~ isUnsignedFlag)
      => DictSetter' tvmk tvmv isUnsignedFlag k v where
    dictSet' :: Dict k v & k & v & s :-> Dict k v & s

instance (ToTVM k ~ 'SliceT, ToTVM v ~ 'SliceT, IsUnsignedTF k ~ 'False, KnownNat (BitSize k))
        => DictSetter' 'SliceT 'SliceT 'False k v where
    dictSet' = do
        pushInt (bitSize @k)
        I DICTSET

instance (ToTVM k ~ 'IntT, ToTVM v ~ 'SliceT, IsUnsignedTF k ~ 'True, KnownNat (BitSize k))
        => DictSetter' 'IntT 'SliceT 'True k v where
    dictSet' = do
        pushInt (bitSize @k)
        I DICTUSET

instance DictSetter' (ToTVM k) (ToTVM v) (IsUnsignedTF k) k v => DictSetter k v where
    dictSet = dictSet'

dsetGet :: forall k s . DictGetter k () => DSet k & k & s :-> Bool & s
dsetGet = do
    dictGet
    ifMaybe (drop >> true) false

dsetSet :: forall k s . (DictSetter k (), ProhibitMaybe (ToTVM k)) => DSet k & k & s :-> DSet k & s
dsetSet = do
    unit
    rollRev @3 -- TODO replace with one roll
    rollRev @3
    dictSet

newDict :: forall k v s . s :-> Dict k v & s
newDict = I NEWDICT

ldDict :: forall k v s . Slice & s :-> Slice & Dict k v & s
ldDict = I LDDICT

stDict :: forall k v s . Builder & Dict k v & s :-> Builder & s
stDict = I STDICT

-- TODO: this function may be extended for DecodeSlice instances
-- but it seems to be unnecessary for our cases
dictRemMin
    :: forall k v s .
    ( DecodeSlice k, DecodeSlice v, KnownNat (BitSize k)
    , DecodeSliceFields k ~ '[k], DecodeSliceFields v ~ '[v]
    , ProhibitMaybe (ToTVM k)
    , ProhibitMaybe (ToTVM v)
    )
    => Dict k v & s :-> (Mb '[ k, v ] & Dict k v & s)
dictRemMin = do
    pushInt (bitSize @k)
    I DICTREMMIN
    fmapMaybe @'[Slice, Slice] @'[k, v] $ do
        decodeFromSliceUnsafe @k
        swap
        decodeFromSliceUnsafe @v
        swap

-- TODO It's bad idea to pass @Dict k v@ to action
-- because the action can accedintally modify it.
-- But I dunno how to eliminate it without DIP.
-- One of options is to put Dict to temporary register c7
dictIter
    :: forall k v s .
    ( DecodeSlice k, DecodeSlice v, KnownNat (BitSize k)
    , DecodeSliceFields k ~ '[k], DecodeSliceFields v ~ '[v]
    , ProhibitMaybe (ToTVM k)
    , ProhibitMaybe (ToTVM v)
    )
    => (k & v & Dict k v & s :-> Dict k v & s)
    -> (Dict k v & s :-> s)
dictIter onEntry = do
    dictRemMin @k @v
    while (I MAYBE_TO_BOOL) $ do
        ifMaybe @'[k, v] onEntry ignore
        dictRemMin @k @v
    ifMaybe (drop >> drop) ignore
    drop