{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PolyKinds #-}

module FiftAsm.DSL
       ( (:->) (..)
       , type (&)
       , (>>)
       , ToTVM

       -- Domain specific types
       , Signature
       , PublicKey
       , Hash
       , RawMsg
       , Slice
       , Cell
       , Builder
       , Timestamp (..)
       , Dictionary
       , DSet

       -- Instructions
       , drop
       , swap
       , dup
       , push
       , pushInt
       , pop
       , move
       , reversePrefix

       , pushRoot
       , popRoot

       , ldSliceX
       , stSlice
       , ld32Unsigned
       , st32Unsigned
       , endS
       , cToS

       , ldDict
       , stDict
       , lookupSet
       , lookupMap
       , dictRemMin
       , dictIter

       , inc
       , equalInt
       , geqInt
       , leqInt
       , greaterInt

       , dataHash
       , cellHash

       , ifMaybe
       , ifElse
       , if_

       , now

       , stacktype
       , cast
       ) where

import Prelude

import qualified Data.Kind as Kind
import GHC.TypeLits (type (+))

import FiftAsm.Instr
import FiftAsm.Types
import Util

type family ToTVM (t :: *) :: T

type family ToTVMs xs where
    ToTVMs '[] = '[]
    ToTVMs (x ': xs) = ToTVM x ': ToTVMs xs

-- | Alias for instruction which hides inner types representation via 'T'.
newtype (inp :: [Kind.Type]) :-> (out :: [Kind.Type]) =
    I { unI :: Instr (ToTVMs inp) (ToTVMs out) }
    deriving (Show)
infixr 1 :->

(>>) :: (a :-> b) -> (b :-> c) -> (a :-> c)
(>>) (I l) (I r) = I (l `Seq` r)

type (&) (a :: Kind.Type) (b :: [Kind.Type]) = a ': b
infixr 2 &

-- Domain specific types
data Signature
data PublicKey
data Hash a
newtype Timestamp = Timestamp Word32
data Dictionary k v
type DSet k = Dictionary k ()
data Cell a
data Builder
data Mb (xs :: [Kind.Type])

-- | RawMsg corresponds to Message object of TVM
-- it contains destination address and body
data RawMsg

-- | Slice represents raw bytes which
-- corresponds to some Haskell datatype.
data Slice

type instance ToTVM Signature = 'SliceT
type instance ToTVM PublicKey = 'IntT
type instance ToTVM (Hash a)  = 'IntT
type instance ToTVM Slice     = 'SliceT
type instance ToTVM RawMsg    = 'SliceT
type instance ToTVM Word32    = 'IntT
type instance ToTVM Timestamp = 'IntT
type instance ToTVM Bits      = 'IntT
type instance ToTVM Bool      = 'IntT
type instance ToTVM Integer   = 'IntT
type instance ToTVM Natural   = 'IntT
type instance ToTVM (Dictionary k v) = 'DictT
type instance ToTVM (Cell a)  = 'CellT
type instance ToTVM Builder   = 'BuilderT
type instance ToTVM ()        = 'NullT
type instance ToTVM (Mb xs)   = 'MaybeT (ToTVMs xs)

-- Type family needed to determin key size of datatype
-- It's needed to perform operations under dictionary.
type family KeySize a :: Nat
type instance KeySize (Hash a) = 256
type instance KeySize Signature = 512
type instance KeySize PublicKey = 256

-- Instructions over :->

drop :: ProhibitMaybe '[ToTVM a] => a & s :-> s
drop = I DROP

dup :: forall a s . ProhibitMaybe '[ToTVM a] => a & s :-> a & a & s
dup = I (PUSH @0)

swap :: ProhibitMaybe '[ToTVM a, ToTVM b] => a & b & s :-> b & a & s
swap = I SWAP

push :: forall (n :: Nat) s .
    ( ProhibitMaybe (Take (n + 1) (ToTVMs s))
    , PushTF n (ToTVMs s) ~ ToTVMs (PushTF n s))
    => s :-> PushTF n s
push = I (PUSH @n)

pushInt :: (Integral a, ToTVM a ~ 'IntT) => a -> (s :-> a & s)
pushInt = I . PUSHINT . toInteger

type family MoveTF (n :: Nat) (xs :: [k]) where
    MoveTF 0 xs = xs
    MoveTF n (y ': xs) = Swap (y ': MoveTF (n - 1) xs)

pop :: forall (n :: Nat) s .
    ( ProhibitMaybe (Take (n + 1) (ToTVMs s))
    , PopTF n (ToTVMs s) ~ ToTVMs (PopTF n s))
    => s :-> PopTF n s
pop = I (POP @n)

-- Move = push s_n; pop s_{n+1}
move :: forall (n :: Nat) s . s :-> MoveTF n s
move = error "not implemented yet" -- push @n >> pop @(n+1)

reversePrefix
    :: forall (n :: Nat) s .
    ( ProhibitMaybe (Take (n + 2) (ToTVMs s))
    , Reverse (Take (n + 2) (ToTVMs s)) ~ ToTVMs (Reverse (Take (n + 2) s))
    )
    => s :-> Reverse (Take (n + 2) s)
reversePrefix = I (REVERSE_PREFIX @n)

pushRoot :: forall a s . s :-> (Cell a & s)
pushRoot = I PUSHROOT

popRoot :: forall a s . (Cell a & s) :-> s
popRoot = I POPROOT

-- pushException :: (Enum e, Exception e) => e -> s :-> Maybe Int & s
-- pushException = pushInt . fromEnum

ldSliceX :: forall a s . ToTVM a ~ 'SliceT => Bits & Slice & s :-> Slice & a & s
ldSliceX = I LDSLICEX

stSlice :: forall a s . ToTVM a ~ 'SliceT => Builder & a & s :-> Builder & s
stSlice = I STSLICE

ld32Unsigned :: forall a s . ToTVM a ~ 'IntT => Slice & s :-> Slice & a & s
ld32Unsigned = I (LDU 31)

st32Unsigned :: forall a s . ToTVM a ~ 'IntT => Builder & a & s :-> Builder & s
st32Unsigned = I (STU 31)

endS :: Slice & s :-> s
endS = I ENDS

cToS :: forall a s . Cell a & s :-> Slice & s
cToS = I CTOS


ldDict :: forall k v s . Slice & s :-> Slice & Dictionary k v & s
ldDict = I LDDICT

stDict :: forall k v s . Builder & Dictionary k v & s :-> Builder & s
stDict = I STDICT

lookupSet :: DSet k & s :-> Bool & s
lookupSet = error "not implemented yet"

lookupMap :: Dictionary k v & s :-> Mb '[v] & s
lookupMap = error "not implemented yet"

-- TODO: this function may be extended for DecodeSlice instances
-- but it seems to be unnecessary for our cases
dictRemMin
    :: forall k v s . (KnownNat (KeySize k), ToTVM k ~ 'SliceT, ToTVM v ~ 'SliceT)
    => Dictionary k v & s :-> (Mb '[ k, v ] & Dictionary k v & s)
dictRemMin = do
    pushInt (natVal @(KeySize k) @Proxy Proxy)
    I DICTREMMIN

-- TODO It's bad idea to pass @Dictionary k v@ to action
-- because the action can accedintally modify it.
-- But I dunno how to eliminate it without DIP.
-- One of options is to put Dictionary to temporary register c7
dictIter
    :: forall k v s . (KnownNat (KeySize k), ToTVM k ~ 'SliceT, ToTVM v ~ 'SliceT)
    => (k & v & Dictionary k v & s :-> Dictionary k v & s)
    -> (Dictionary k v & s :-> s)
dictIter onEntry = do
    while (dictRemMin @k @v >> I MAYBE_TO_BOOL) $
        ifMaybe @'[k, v] onEntry (I Ignore)
    drop

inc :: ToTVM a ~ 'IntT => a & s :-> a & s
inc = I INC

equalInt :: ToTVM a ~ 'IntT => a & a & s :-> Bool & s
equalInt = I EQUAL

geqInt :: ToTVM a ~ 'IntT => a & a & s :-> Bool & s
geqInt = I GEQ

greaterInt :: ToTVM a ~ 'IntT => a & a & s :-> Bool & s
greaterInt = I GREATER

leqInt :: ToTVM a ~ 'IntT => a & a & s :-> Bool & s
leqInt = I LEQ

dataHash :: ToTVM a ~ 'SliceT => a & s :-> Hash a & s
dataHash = I SHA256U

cellHash :: Cell a & s :-> Hash a & s
cellHash = I HASHCU

-- if statements
ifMaybe :: forall a s t . (ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s))
        => (a ++ s :-> t) -> (s :-> t) -> (Mb a & s :-> t)
ifMaybe (I t) (I f) = I (IF_MAYBE t f)

ifElse  :: (s :-> t) -> (s :-> t)  -> (Bool & s :-> t)
ifElse (I t) (I f) = I (IFELSE t f)

if_ :: (s :-> t) -> (Bool & s :-> t)
if_ (I t) = I (IF t)

while :: (s :-> Bool & t) -> (t :-> s) -> (s :-> s)
while (I st) (I body) = I (WHILE st body)

-- Application specific instructions
now :: s :-> Timestamp & s
now = I NOW

-- Auxiliary DSL instructions
stacktype :: forall s . s :-> s
stacktype = I Ignore

cast :: forall a b s . a & s :-> b & s
cast = I Ignore