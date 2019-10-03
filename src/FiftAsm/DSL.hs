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
       , Timestamp (..)
       , Dictionary
       , DSet

       -- Instructions
       , pushRoot
       , drop
       , endS
       , swap
       , dup
       , push
       , pushInt
       , pop
       , move
       , ldSliceX
       , ldDict
       , lookupSet
       , lookupMap
       , now
       , ifMaybe
       , ifElse
       , if_
       , stacktype
       , cast
       ) where


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
type instance ToTVM (Dictionary k v) = 'DictT
type instance ToTVM () = 'NullT
type instance ToTVM (Maybe a) = 'MaybeT (ToTVM a)

-- Instructions over :->

pushRoot :: s :-> (Slice & s)
pushRoot = I (PUSHROOT `Seq` CTOS)

drop :: ProhibitMaybe '[ToTVM a] => a & s :-> s
drop = I DROP

dup :: forall a s . ProhibitMaybe '[ToTVM a] => a & s :-> a & a & s
dup = I (PUSH @0)

endS :: Slice & s :-> s
endS = I ENDS

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

-- pushException :: (Enum e, Exception e) => e -> s :-> Maybe Int & s
-- pushException = pushInt . fromEnum

ldSliceX :: forall a s . ToTVM a ~ 'SliceT => Bits & Slice & s :-> Slice & a & s
ldSliceX = I LDSLICEX

ldDict :: forall k v s . Slice & s :-> Slice & Dictionary k v & s
ldDict = I LDDICT

lookupSet :: DSet (Hash a) & s :-> Bool & s
lookupSet = error "not implemented yet"

lookupMap :: Dictionary (Hash a) b & s :-> Maybe b & s
lookupMap = error "not implemented yet"

now :: s :-> Timestamp & s
now = I NOW

-- ldRefRToS :: Slice & s :-> Slice & Slice & s
-- ldRefRToS = I LDREFRTOS

-- sChkBitsQ :: (Bits & Slice & s) :-> (Bool & s)
-- sChkBitsQ  = I SCHKBITSQ

-- if statements
ifMaybe :: (a & s :-> t) -> (s :-> t) -> (Maybe a & s :-> t)
ifMaybe (I t) (I f) = I (IF_MAYBE t f)

ifElse  :: (s :-> t) -> (s :-> t)  -> (Bool & s :-> t)
ifElse (I t) (I f) = I (IFELSE t f)

if_ :: (s :-> t) -> (Bool & s :-> t)
if_ (I t) = I (IF t)

stacktype :: forall s . s :-> s
stacktype = I Ignore

cast :: forall a b s . a & s :-> b & s
cast = I Ignore