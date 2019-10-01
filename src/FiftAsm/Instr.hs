{-# LANGUAGE KindSignatures #-}

module FiftAsm.Instr
    ( Instr (..)
    , Bits (..)
    ) where

import GHC.TypeLits (Nat, TypeError, ErrorMessage (..), type (-))

import FiftAsm.Types

newtype Bits = Bits Word32
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

data Instr (inp :: [T]) (out :: [T]) where
    Seq      :: Instr a b -> Instr b c -> Instr a c -- bind two programs
    Ignore   :: Instr a b -- will be ingored when printed

    SWAP     :: Instr (a ': b ': s) (b ': a ': s)
    PUSH     :: forall (n :: Nat) s . Instr s (PushTF n s)
    PUSHINT  :: Integer -> Instr s ('IntT ': s)
    PUSHROOT :: Instr s ('CellT ': s)
    DROP     :: Instr (a ': s) s

    -- cell serialization (Builder manipulation primitives)
    NEWC     :: Instr s ('BuilderT ': s)
    ENDC     :: Instr ('BuilderT ': s) ('CellT ': s)
    STU      :: Bits -> Instr ('BuilderT ': 'IntT ': s) ('BuilderT ': s)

    -- cell deserialization (CellSlice primitives)
    CTOS     :: Instr ('CellT ': s) ('SliceT ': s)
    ENDS     :: Instr ('SliceT ': s) s
    LDU      :: Bits -> Instr ('SliceT ': s) ('SliceT ': 'IntT ': s)
    LDSLICE  :: Bits -> Instr ('SliceT ': s) ('SliceT ': 'SliceT ': s)
    LDSLICEX :: Instr ('IntT ': 'SliceT ': s) ('SliceT ': 'SliceT ': s)
    -- LDREFRTOS :: Instr ('SliceT ': s) ('SliceT ': 'SliceT ': s)
    -- SCHKBITSQ :: Instr ('IntT ': 'SliceT ': s) ('IntT ': s)

    -- dict primitives
    LDDICT :: Instr ('SliceT ': s) ('SliceT ': 'DictT ': s)
    DICTGET :: Instr ('IntT ': 'DictT ': 'SliceT ': s) ('MaybeT 'SliceT ': s)

deriving instance Show (Instr a b)

-- Auxiliary type families
type family Head (xs :: [T]) where
    Head (x ': _) = x
    Head _ = TypeError ('Text "Head doesn't exist")

type family Swap (xs :: [T]) where
    Swap (x ': y ': s) = y ': x ': s
    Swap _ = TypeError ('Text "Swap: at least two elements don't exist")

type family PushTF (n :: Nat) (xs :: [T]) where
    PushTF 0 (x ': xs) = x ': x ': xs
    PushTF n (y ': xs) = Swap (y ': PushTF (n - 1) xs)