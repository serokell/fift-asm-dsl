{-# LANGUAGE PolyKinds #-}

module FiftAsm.Instr
    ( Instr (..)
    , Bits (..)
    , ProhibitMaybe
    , PushTF
    , PopTF
    ) where

import GHC.TypeLits (TypeError, ErrorMessage (..), type (-), type (+))
import Data.Vinyl.TypeLevel (type (++))

import FiftAsm.Types
import Util

newtype Bits = Bits Word32
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

data Instr (inp :: [T]) (out :: [T]) where
    Seq      :: Instr a b -> Instr b c -> Instr a c -- bind two programs
    Ignore   :: Instr a b -- will be ingored when printed

    SWAP     :: ProhibitMaybe '[a, b] => Instr (a ': b ': s) (b ': a ': s)
    PUSH     :: forall (n :: Nat) s . ProhibitMaybe (Take (n + 1) s) => Instr s (PushTF n s)
    POP      :: forall (n :: Nat) s . ProhibitMaybe (Take (n + 1) s) => Instr s (PopTF n s)
    PUSHINT  :: Integer -> Instr s ('IntT ': s)
    DROP     :: ProhibitMaybe '[a] => Instr (a ': s) s
    -- Custom instruction which is translated to REVERSE i+2, j
    REVERSE_PREFIX
        :: forall (n :: Nat) s . ProhibitMaybe (Take (n + 2) s)
        => Instr s (Reverse (Take (n + 2) s))


    PUSHROOT :: Instr s ('CellT ': s)
    POPROOT  :: Instr ('CellT ': s) s

    -- Comparison primitives
    EQUAL    :: Instr ('IntT ': 'IntT ': s) ('IntT ': s)
    GEQ      :: Instr ('IntT ': 'IntT ': s) ('IntT ': s)
    LEQ      :: Instr ('IntT ': 'IntT ': s) ('IntT ': s)
    GREATER  :: Instr ('IntT ': 'IntT ': s) ('IntT ': s)

    -- cell serialization (Builder manipulation primitives)
    NEWC     :: Instr s ('BuilderT ': s)
    ENDC     :: Instr ('BuilderT ': s) ('CellT ': s)
    STU      :: Bits -> Instr ('BuilderT ': 'IntT ': s) ('BuilderT ': s)
    STSLICE  :: Instr ('BuilderT ': 'SliceT ': s) ('BuilderT ': s)

    -- cell deserialization (CellSlice primitives)
    CTOS     :: Instr ('CellT ': s) ('SliceT ': s)
    ENDS     :: Instr ('SliceT ': s) s
    LDU      :: Bits -> Instr ('SliceT ': s) ('SliceT ': 'IntT ': s)
    LDSLICE  :: Bits -> Instr ('SliceT ': s) ('SliceT ': 'SliceT ': s)
    LDSLICEX :: Instr ('IntT ': 'SliceT ': s) ('SliceT ': 'SliceT ': s)
    -- LDREFRTOS :: Instr ('SliceT ': s) ('SliceT ': 'SliceT ': s)
    -- SCHKBITSQ :: Instr ('IntT ': 'SliceT ': s) ('IntT ': s)

    -- dict primitives
    LDDICT  :: Instr ('SliceT ': s) ('SliceT ': 'DictT ': s)
    DICTGET :: Instr ('IntT ': 'DictT ': 'SliceT ': s) ('MaybeT 'SliceT ': s)
    STDICT  :: Instr ('BuilderT ': 'DictT ': s) ('BuilderT ': s)

    NOW :: Instr s ('IntT ': s)

    -- if statements
    IF_MAYBE :: Instr (a ': s) t -> Instr s t -> Instr ('MaybeT a ': s) t
    IFELSE   :: Instr s t -> Instr s t -> Instr ('IntT ': s) t
    IF       :: Instr s t -> Instr ('IntT ': s) t

    -- hashes
    HASHCU  :: Instr ('CellT ': s) ('IntT ': s)  -- hashing a Cell
    SHA256U :: Instr ('SliceT ': s) ('IntT ': s) -- hashing only Data bits of slice

deriving instance Show (Instr a b)

type PopTF n s = (Take n s ++ Drop (n + 1) s)

type family PushTF (n :: Nat) (xs :: [k]) where
    PushTF 0 (x ': xs) = x ': x ': xs
    PushTF n (y ': xs) = Swap (y ': PushTF (n - 1) xs)

type family ProhibitMaybe (xs :: [T]) :: Constraint where
    ProhibitMaybe '[] = ()
    ProhibitMaybe ('MaybeT _ ': xs) =
        TypeError ('Text "This operation is not permitted due to presence Maybe value on the stack.")
    ProhibitMaybe (_ ': xs) = ProhibitMaybe xs