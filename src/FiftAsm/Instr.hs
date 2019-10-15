{-# LANGUAGE PolyKinds #-}

module FiftAsm.Instr
    ( Instr (..)
    , Bits (..)
    , ProhibitMaybe
    , ProhibitMaybes
    , ProhibitMaybeTF
    , PushTF
    , PopTF
    , RollRevTF
    , RollTF
    , XchgTF
    , Xchg2TF
    ) where

import GHC.TypeLits (TypeError, ErrorMessage (..), type (-), type (+), type (<=))
import GHC.TypeLits.Extra (Max, Min)
import Data.Vinyl.TypeLevel (type (++))
import Fmt (Buildable)

import FiftAsm.Types
import Util

newtype Bits = Bits Word32
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Buildable)

type (&) (a :: T) (b :: [T]) = a ': b
infixr 2 &

data Instr (inp :: [T]) (out :: [T]) where
    Seq      :: Instr a b -> Instr b c -> Instr a c -- bind two programs
    Ignore   :: Instr a b                           -- will be ingored when printed
    Comment  :: Text -> Instr a a                   -- print comment

    SWAP     :: ProhibitMaybes '[a, b] => Instr (a & b & s) (b & a & s)
    PUSH     :: forall (i :: Nat) s . (ProhibitMaybes (Take (i + 1) s), KnownNat i) => Proxy i -> Instr s (PushTF i s)
    POP      :: forall (i :: Nat) s . (ProhibitMaybes (Take (i + 1) s), KnownNat i) => Proxy i -> Instr s (PopTF i s)
    PUSHINT  :: Integer -> Instr s ('IntT & s)
    TRUE     :: Instr s ('IntT & s)
    FALSE    :: Instr s ('IntT & s)
    DROP     :: ProhibitMaybe a => Instr (a & s) s
    ROLL
        :: forall (n :: Nat) s . (ProhibitMaybes (Take n s), 1 <= n, KnownNat n)
        => Proxy n -> Instr s (RollTF n s)
    ROLLREV
        :: forall (n :: Nat) s . (ProhibitMaybes (Take n s), 1 <= n, KnownNat n)
        => Proxy n -> Instr s (RollRevTF n s)
    -- Custom instruction which is translated to REVERSE i+2, j
    REVERSE_PREFIX
        :: forall (n :: Nat) s . (ProhibitMaybes (Take n s), 2 <= n, KnownNat n)
        => Proxy n -> Instr s (Reverse (Take n s) ++ Drop n s)
    XCHG
      :: forall (i :: Nat) s .
      ( ProhibitMaybes (Take i s), 1 <= i, KnownNat i )
      => Proxy i -> Instr s (XchgTF 0 i s)
    XCHG'
      :: forall (i :: Nat) (j :: Nat) s .
      ( ProhibitMaybes (Take (Max i j) s), KnownNat i, KnownNat j
      ) => Proxy i -> Proxy j -> Instr s (XchgTF i j s)
    XCHG2
      :: forall (i :: Nat) (j :: Nat) s .
      ( ProhibitMaybes (Take (Max i j) s), KnownNat i, KnownNat j
      ) => Proxy i -> Proxy j -> Instr s (Xchg2TF i j s)
    XCPU
      :: forall (i :: Nat) (j :: Nat) s .
      ( ProhibitMaybes (Take (Max i j) s), KnownNat i, KnownNat j
      ) => Proxy i -> Proxy j -> Instr s (PushTF j (XchgTF 0 i s))
    XC2PU
      :: forall (i :: Nat) (j :: Nat) (k :: Nat) s .
      ( ProhibitMaybes (Take (Max (Max i j) k) s)
      , KnownNat i, KnownNat j, KnownNat k
      ) => Proxy i -> Proxy j -> Proxy k -> Instr s (PushTF k (Xchg2TF i j s))



    PUSHROOT :: Instr s ('CellT & s)
    POPROOT  :: Instr ('CellT & s) s

    -- Arithmetic and comparison primitives
    INC      :: Instr ('IntT & s) ('IntT & s)
    ADD      :: Instr ('IntT & 'IntT & s) ('IntT & s)
    EQUAL    :: Instr ('IntT & 'IntT & s) ('IntT & s)
    GEQ      :: Instr ('IntT & 'IntT & s) ('IntT & s)
    LEQ      :: Instr ('IntT & 'IntT & s) ('IntT & s)
    GREATER  :: Instr ('IntT & 'IntT & s) ('IntT & s)
    LESS  :: Instr ('IntT & 'IntT & s) ('IntT & s)

    LSHIFT   :: Instr ('IntT & 'IntT & s) ('IntT & s)
    RSHIFT   :: Instr ('IntT & 'IntT & s) ('IntT & s)


    -- cell serialization (Builder manipulation primitives)
    NEWC     :: Instr s ('BuilderT & s)
    ENDC     :: Instr ('BuilderT & s) ('CellT & s)
    STU      :: Bits -> Instr ('BuilderT & 'IntT & s) ('BuilderT & s)
    STSLICE  :: Instr ('BuilderT & 'SliceT & s) ('BuilderT & s)
    STREF    :: Instr ('BuilderT & 'CellT & s) ('BuilderT & s)

    -- cell deserialization (CellSlice primitives)
    CTOS     :: Instr ('CellT & s) ('SliceT & s)
    ENDS     :: Instr ('SliceT & s) s
    LDU      :: Bits -> Instr ('SliceT & s) ('SliceT & 'IntT & s)
    PLDU     :: Bits -> Instr ('SliceT & s) ('IntT & s)
    -- LDSLICE  :: Bits -> Instr ('SliceT & s) ('SliceT & 'SliceT & s)
    LDSLICEX :: Instr ('IntT & 'SliceT & s) ('SliceT & 'SliceT & s)
    PLDSLICEX :: Instr ('IntT & 'SliceT & s) ('SliceT & s)
    LDREF    :: Instr ('SliceT & s) ('SliceT & 'CellT & s)
    PLDREF   :: Instr ('SliceT & s) ('CellT & s)
    SREFS :: Instr ('SliceT & s) ('IntT & s)

    -- dict primitives
    NEWDICT :: Instr s ('DictT & s)
    DICTEMPTY :: Instr ('DictT & s) ('IntT & s)
    LDDICT  :: Instr ('SliceT & s) ('SliceT & 'DictT & s)
    PLDDICT  :: Instr ('SliceT & s) ('DictT & s)
    DICTGET :: Instr ('IntT & 'DictT & 'SliceT & s) ('MaybeT '[ 'SliceT ] & s)
    DICTUGET :: Instr ('IntT & 'DictT & 'IntT & s) ('MaybeT '[ 'SliceT ] & s)
    STDICT  :: Instr ('BuilderT & 'DictT & s) ('BuilderT & s)
    DICTREMMIN :: Instr ('IntT & 'DictT & s) ('MaybeT '[ 'SliceT, 'SliceT ] & 'DictT & s)
    DICTSET  :: Instr ('IntT & 'DictT & 'SliceT & 'SliceT & s) ('DictT & s)
    DICTUSET :: Instr ('IntT & 'DictT & 'IntT & 'SliceT & s) ('DictT & s)
    DICTDEL  :: Instr ('IntT & 'DictT & 'SliceT & s) ('IntT & 'DictT & s)
    DICTUDEL :: Instr ('IntT & 'DictT & 'IntT & s) ('IntT & 'DictT & s)

    -- This instruction doesn't exist in Fift Assembler
    -- but it can be easily implemented as DUP
    MAYBE_TO_BOOL :: Instr ('MaybeT a & s) ('IntT & 'MaybeT a & s)

    -- if statements
    IF_JUST  :: Instr (a ++ s) t -> Instr s t -> Instr ('MaybeT a & s) t
    FMAP_MAYBE :: Instr (a ++ s) (b ++ s) -> Instr ('MaybeT a & s) ('MaybeT b & s)
    JUST     :: Instr (a ++ s) ('MaybeT a & s)
    NOTHING  :: Instr s ('MaybeT a & s)
    IFELSE   :: Instr s t -> Instr s t -> Instr ('IntT & s) t

    -- control flow statements
    WHILE      :: Instr s ('IntT & s) -> Instr s s -> Instr s s
    THROWIFNOT :: (Enum e, Exception e) => e -> Instr ('IntT & s) s
    THROWIF    :: (Enum e, Exception e) => e -> Instr ('IntT & s) s
    THROW      :: (Enum e, Exception e) => e -> Instr s t

    -- hashes
    HASHCU  :: Instr ('CellT & s) ('IntT & s)  -- hashing a Cell
    HASHSU  :: Instr ('SliceT & s) ('IntT & s)  -- hashing a Slice
    SHA256U :: Instr ('SliceT & s) ('IntT & s) -- hashing only Data bits of slice
    CHKSIGNS :: Instr ('IntT & 'SliceT & 'SliceT & s) ('IntT & s)
    CHKSIGNU :: Instr ('IntT & 'SliceT & 'IntT & s) ('IntT & s)

    NOW :: Instr s ('IntT & s)
    SENDRAWMSG :: Instr ('IntT & 'CellT & s) s
    ACCEPT :: Instr s s

    PAIR :: Instr (a & b & s) ('TupleT [a, b] & s)
    UNPAIR :: Instr ('TupleT [a, b] & s) (a & b & s)

    CALL :: String -> Instr s t

deriving instance Show (Instr a b)

type PopTF n s = (Tail (Take n s) ++ '[Head s] ++ Drop (n + 1) s)

type family PushTF (n :: Nat) (xs :: [k]) where
    PushTF 0 (x ': xs) = x ': x ': xs
    PushTF n (y ': xs) = Swap (y ': PushTF (n - 1) xs)

type RollTF n s = Head (Drop n s) ': Take n s ++ Drop (n + 1) s

type RollRevTF n s = Take n (Drop 1 s) ++ (Head s ': Drop (n + 1) s)

type XchgTF i j s =
  Take (Min i j) s ++ (Head (Drop (Max i j) s) ': Take (Max i j - Min i j - 1) (Drop (Min i j + 1) s) ++ (Head (Drop (Min i j) s) ': Drop (Max i j + 1) s))

type Xchg2TF i j s = XchgTF 0 j (XchgTF 1 i s)

type ProhibitMaybes (xs :: [T]) = RecAll_ xs ProhibitMaybe

class ProhibitMaybe (x :: T)
instance ProhibitMaybeTF x => ProhibitMaybe x

type family ProhibitMaybeTF (x :: T) :: Constraint where
    ProhibitMaybeTF ('MaybeT x) =
        TypeError ('Text "This operation is not permitted due to presence Maybe value on the stack.")
    ProhibitMaybeTF _ = ()
