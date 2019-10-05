{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PolyKinds #-}

module FiftAsm.Dictionary
       ( Dict
       , DSet
       , Size
       , newDict
       , ldDict
       , stDict
       , dictGet
       , dictRemMin
       , dictIter
       , dictSet
       , dsetGet
       , dsetSet
       , dictDelIgnore
       , dictEmpty
       , dickSize
       , dictMerge
       ) where

import Prelude

import FiftAsm.Instr
import FiftAsm.Types
import FiftAsm.DSL
import FiftAsm.Cell

data Dict k v
type DSet k = Dict k ()
type instance ToTVM (Dict k v) = 'DictT

newtype Size = Size Word32
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
type instance ToTVM Size = 'IntT

instance DecodeSlice (Dict k v) where
    decodeFromSlice = ldDict

instance EncodeBuilder (Dict k v) where
    encodeToBuilder = stDict

-- Getter / Setter primitives

class DictOperations k v where
    dictGet :: Dict k v & k & s :-> Mb '[v] & s
    dictSet :: Dict k v & k & v & s :-> Dict k v & s
    dictDel :: Dict k v & k & s :-> Bool & Dict k v & s

class (ToTVM k ~ tvmk, ToTVM v ~ tvmv, IsUnsignedTF k ~ isUnsignedFlag)
      => DictOperations' tvmk tvmv isUnsignedFlag k v where
    dictGet' :: Dict k v & k & s :-> Mb '[v] & s
    dictSet' :: Dict k v & k & v & s :-> Dict k v & s
    dictDel' :: Dict k v & k & s :-> Bool & Dict k v & s

instance (ToTVM k ~ 'SliceT, ToTVM v ~ 'SliceT, IsUnsignedTF k ~ 'False, KnownNat (BitSize k))
        => DictOperations' 'SliceT 'SliceT 'False k v where
    dictGet' = do
        pushInt (bitSize @k)
        I DICTGET
    dictSet' = do
        pushInt (bitSize @k)
        I DICTSET
    dictDel' = do
        pushInt (bitSize @k)
        I DICTDEL

instance (ToTVM k ~ 'IntT, ToTVM v ~ 'SliceT, IsUnsignedTF k ~ 'True, KnownNat (BitSize k))
        => DictOperations' 'IntT 'SliceT 'True k v where
    dictGet' = do
        pushInt (bitSize @k)
        I DICTUGET
    dictSet' = do
        pushInt (bitSize @k)
        I DICTUSET
    dictDel' = do
        pushInt (bitSize @k)
        I DICTUDEL

instance DictOperations' (ToTVM k) (ToTVM v) (IsUnsignedTF k) k v => DictOperations k v where
    dictGet = dictGet'
    dictSet = dictSet'
    dictDel = dictDel'

dsetGet :: forall k s . DictOperations k () => DSet k & k & s :-> Bool & s
dsetGet = do
    dictGet
    if IsJust then drop >> true
    else false

dsetSet :: forall k s . (DictOperations k (), ProhibitMaybe (ToTVM k)) => DSet k & k & s :-> DSet k & s
dsetSet = do
    unit
    roll @3
    dictSet

dictDelIgnore :: forall k v s . DictOperations k v => Dict k v & k & s :-> Dict k v & s
dictDelIgnore = do
    dictDel
    drop

newDict :: forall k v s . s :-> Dict k v & s
newDict = I NEWDICT

ldDict :: forall k v s . Slice & s :-> Slice & Dict k v & s
ldDict = I LDDICT

stDict :: forall k v s . Builder & Dict k v & s :-> Builder & s
stDict = I STDICT

type DictRemMinC k v =
    ( DecodeSlice k, DecodeSlice v, KnownNat (BitSize k)
    , DecodeSliceFields k ~ '[k], DecodeSliceFields v ~ '[v]
    , ProhibitMaybe (ToTVM k), ProhibitMaybe (ToTVM v)
    )

-- TODO: this function may be extended for DecodeSlice instances
-- but it seems to be unnecessary for our cases
dictRemMin
    :: forall k v s . DictRemMinC k v
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
    :: forall k v s . DictRemMinC k v
    => (k & v & Dict k v & s :-> Dict k v & s)
    -> (Dict k v & s :-> s)
dictIter onEntry = do
    comment "Iterate over dictionary"
    dictRemMin @k @v
    while (I MAYBE_TO_BOOL) $ do
        ifJust @'[k, v] onEntry ignore
        dictRemMin @k @v
    if IsJust then
        drop >> drop
    else
        ignore
    drop

dictEmpty :: Dict k v & s :-> Bool & s
dictEmpty = I DICTEMPTY

-- Returns either Just Size or Nothing if size is not less than passed K.
dickSize
    :: forall k v s . DictRemMinC k v
    => Dict k v & Word32 & s :-> Mb '[Size] & s
dickSize = do
    comment "Compute dictionary size"
    pushInt @Size 0
    swap
    dictIter $ do
        drop
        drop
        swap
        inc
        dup
        stacktype' @[Size, Size, Dict k v, Word32]
        cast @Size @Word32
        push @3
        leqInt
        -- if size is greater than k, let's replace a dict with empt one to stop iteration
        if NotHolds then swap
        else do
            swap
            drop
            newDict @k @v
    dup
    roll @3
    stacktype' @[Size, Word32, Size]
    cast @Size @Word32
    geqInt
    if Holds then
        drop >> nothing @'[Size]
    else
        just @'[Size]

-- Returns either Just (Dict k v) or Nothing if size of Dict k v is not less than passed K.
dictMerge
    :: forall k v s .
    ( DictRemMinC k v
    , DictOperations k v
    )
    => Dict k v & Dict k v & Word32 & s :-> Mb '[Dict k v] & s
dictMerge = do
    comment "Merge two dictionaries"
    dup
    dictEmpty

    -- If the first one is empty let's return the second one
    -- computing its size beforehand
    if Holds then do
        drop
        dup
        roll @3
        dickSize
        if IsJust then
            drop >> just @'[Dict k v]
        else
            drop >> nothing
    else do
        dup
        push @3
        swap
        dickSize
        stacktype' @[Mb '[Size], Dict k v, Dict k v, Word32]
        -- If size of the first one is not less that K, then just return Nothing
        if IsNothing then
            drop >> drop >> drop >> nothing
        else do
            -- Otherwise let's iter over the second one and add to the first one elements
            -- which don't present there
            moveOnTop @2
            stacktype' @[Dict k v, Size, Dict k v, Word32]
            dictIter $ do
                dup
                push @3
                dictGet
                stacktype' @[Mb '[v], k, v, Dict k v, Size, Dict k v, Word32]
                -- Check whether a key is already in the first dict
                if IsJust then
                    drop >> drop >> drop
                else do
                    moveOnTop @4
                    dictSet
                    rollRev @3
                    inc
                    dup
                    cast @Size @Word32
                    push @4
                    leqInt
                    -- Check if a current size is not less than k
                    if NotHolds then
                        rollRev @3
                    else do
                        rollRev @3
                        drop
                        newDict @k @v
            stacktype' @[Size, Dict k v, Word32]
            cast @Size @Word32
            rollRev @3
            if IsLe then
                drop >> nothing
            else
                just