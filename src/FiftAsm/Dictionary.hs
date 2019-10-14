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
       , dictSize
       , dictMerge

       , dictEncodeSet
       ) where

import Data.Vinyl.TypeLevel (type (++))
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
    decodeFromSliceImpl = ldDict

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

instance
  ( ToTVM k ~ 'SliceT
  , ToTVM v ~ 'SliceT
  , IsUnsignedTF k ~ 'False
  , KnownNat (BitSize k)
  )
  => DictOperations' 'SliceT 'SliceT 'False k v where
    dictGet' = do
        pushInt (bitSize @k)
        mkI DICTGET
    dictSet' = do
        pushInt (bitSize @k)
        mkI DICTSET
    dictDel' = do
        pushInt (bitSize @k)
        mkI DICTDEL

instance (ToTVM k ~ 'IntT, ToTVM v ~ 'SliceT, IsUnsignedTF k ~ 'True, KnownNat (BitSize k))
        => DictOperations' 'IntT 'SliceT 'True k v where
    dictGet' = do
        pushInt (bitSize @k)
        mkI DICTUGET
    dictSet' = do
        pushInt (bitSize @k)
        mkI DICTUSET
    dictDel' = do
        pushInt (bitSize @k)
        mkI DICTUDEL

instance DictOperations' (ToTVM k) (ToTVM v) (IsUnsignedTF k) k v => DictOperations k v where
    dictGet = dictGet'
    dictSet = dictSet'
    dictDel = dictDel'

dictEncodeSet
  :: forall k v s .
    ( EncodeBuilder v
    , ProhibitMaybeTF (ToTVM v)
    , ProhibitMaybeTF (ToTVM k)
    , DictOperations k v
    )
  => EncodeBuilderFields v ++ (k & Dict k v & s) :-> Dict k v & s
dictEncodeSet = do
    encodeToSlice @v @(k & Dict k v & s)
    cast @Slice @v
    reversePrefix @3
    dictSet @k @v

dsetGet :: forall k s . DictOperations k () => DSet k & k & s :-> Bool & s
dsetGet = do
    dictGet
    if IsJust then drop >> true
    else false

dsetSet :: forall k s . (DictOperations k (), ProhibitMaybe (ToTVM k)) => DSet k & k & s :-> DSet k & s
dsetSet = do
    unit
    rollRev @2
    dictSet

dictDelIgnore :: forall k v s . DictOperations k v => Dict k v & k & s :-> Dict k v & s
dictDelIgnore = do
    dictDel
    drop

newDict :: forall k v s . s :-> Dict k v & s
newDict = mkI NEWDICT

ldDict :: forall k v s . Slice & s :-> Slice & Dict k v & s
ldDict = mkI LDDICT

stDict :: forall k v s . Builder & Dict k v & s :-> Builder & s
stDict = mkI STDICT

type DictRemMinC k v =
    ( DecodeSlice k, DecodeSlice v, KnownNat (BitSize k)
    , DecodeSliceFields k ~ '[k]
    , ProhibitMaybe (ToTVM k)
    , Typeable k
    , Typeable v
    , (DecodeSliceFields v ++ '[]) ~ DecodeSliceFields v
    , Typeable (DecodeSliceFields v)
    )

-- TODO: this function may be extended for DecodeSlice instances
-- but it seems to be unnecessary for our cases
dictRemMin
    :: forall k v s . DictRemMinC k v
    => Dict k v & s :-> (Mb '[ k, Slice ] & Dict k v & s)
dictRemMin =
  viaSubroutine @'[Dict k v]
                @'[Mb '[ k, Slice ], Dict k v]
                "dictRemMin" $ do
    pushInt (bitSize @k)
    mkI DICTREMMIN
    fmapMaybe @'[Slice, Slice] @'[k, Slice] $ decodeFromSliceUnsafe @k

-- TODO It's bad idea to pass @Dict k v@ to action
-- because the action can accedintally modify it.
-- But I dunno how to eliminate it without DIP.
-- One of options is to put Dict to temporary register c7
dictIter'
    :: forall k v s . DictRemMinC k v
    => k & Slice & Dict k v & s :-> Dict k v & s
    -> Dict k v & s :-> s
dictIter' onEntry = do
    comment "Iterate over dictionary"
    dictRemMin @k @v
    while (mkI MAYBE_TO_BOOL) $ do
        ifJust @'[k, Slice] onEntry ignore
        dictRemMin @k @v
    if IsJust then
        drop >> drop
    else
        ignore
    drop

dictIter
    :: forall k v s . DictRemMinC k v
    => DecodeSliceFields v ++ (k & Dict k v & s) :-> Dict k v & s
    -> Dict k v & s :-> s
dictIter onEntry = dictIter' $ do
    swap
    decodeFromSliceUnsafe @v
    onEntry

dictEmpty :: Dict k v & s :-> Bool & s
dictEmpty = mkI DICTEMPTY

-- Returns either Just Size or Nothing if size is not less than passed K.
dictSize
    :: forall k v s . DictRemMinC k v
    => Dict k v & Word32 & s :-> Mb '[Size] & s
dictSize = viaSubroutine @'[Dict k v, Word32] @'[Mb '[Size]] "dictSize" $ do
    comment "Compute dictionary size"
    pushInt @Size 0
    swap
    dictIter' $ do
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
    rollRev @2
    stacktype' @[Size, Word32, Size]
    cast @Size @Word32
    geqInt
    if Holds then
        drop >> nothing @'[Size]
    else
        just @'[Size]

-- Returns either Just (Dict k v) or Nothing if size of Dict k v is >= K.
dictMerge
    :: forall k v s .
    ( DictRemMinC k v
    , DictOperations k v
    , DecodeSliceFields v ~ '[v] -- TODO this restriction can be made lighter
    , ProhibitMaybe (ToTVM v)
    )
    => Dict k v & Dict k v & Word32 & s :-> Mb '[Dict k v] & s
dictMerge = do
  viaSubroutine @'[Dict k v, Dict k v, Word32]
                @'[Mb '[Dict k v]]
                "dictMerge" $ do
    comment "Merge two dictionaries"
    dup
    dictEmpty

    -- If the first one is empty let's return the second one
    -- computing its size beforehand
    if Holds then do
        drop
        dup
        rollRev @2
        dictSize
        if IsJust then
            drop >> just @'[Dict k v]
        else
            drop >> nothing
    else do
        dup
        push @3
        swap
        dictSize
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
                swap
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
                    roll @2
                    inc
                    dup
                    cast @Size @Word32
                    push @4
                    leqInt
                    -- Check if a current size is not less than k
                    if NotHolds then
                        roll @2
                    else do
                        roll @2
                        drop
                        newDict @k @v
            stacktype' @[Size, Dict k v, Word32]
            cast @Size @Word32
            roll @2
            if IsLe then
                drop >> nothing
            else
                just
