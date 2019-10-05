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
       , MessageObject
       , Slice
       , Cell
       , Builder
       , Timestamp (..)
       , Mb
       , BitSize
       , bitSize
       , IsUnsignedTF
       , IsUnsigned

       -- Instructions
       , drop
       , swap
       , dup
       , push
       , pushInt
       , unit
       , true
       , false
       , pop
       , moveOnTop
       , rollRev
       , roll
       , reversePrefix

       , pushRoot
       , popRoot

       , ldSliceX
       , stSlice
       , ld32Unsigned
       , st32Unsigned
       , endS
       , cToS

       , inc
       , equalInt
       , geqInt
       , leqInt
       , greaterInt

       , dataHash
       , cellHash
       , chkSignS
       , chkSignU

       , ifJust
       , ifNothing
       , fmapMaybe
       , just
       , nothing
       , ifElse
       , ifThenElse
       , Condition (..)
       , while
       , throwIf
       , throwIfNot

       , now
       , sendRawMsg

       , stacktype
       , stacktype'
       , cast
       , ignore
       , comment
       ) where

import Prelude

import qualified Data.Kind as Kind
import GHC.TypeLits (type (+), type (<=))

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

data Cell a
data Builder
data Mb (xs :: [Kind.Type])

-- | RawMsg corresponds to Message object of TVM
-- it contains destination address and body
data MessageObject

-- | Slice represents raw bytes which
-- corresponds to some Haskell datatype.
data Slice

type instance ToTVM Signature = 'SliceT
type instance ToTVM PublicKey = 'IntT
type instance ToTVM (Hash a)  = 'IntT
type instance ToTVM Slice     = 'SliceT
type instance ToTVM Word32    = 'IntT
type instance ToTVM Timestamp = 'IntT
type instance ToTVM Bits      = 'IntT
type instance ToTVM Bool      = 'IntT
type instance ToTVM Integer   = 'IntT
type instance ToTVM Natural   = 'IntT
type instance ToTVM (Cell a)  = 'CellT
type instance ToTVM Builder   = 'BuilderT
type instance ToTVM ()        = 'SliceT
type instance ToTVM (Mb xs)   = 'MaybeT (ToTVMs xs)

type family IsUnsignedTF a :: Bool where
    IsUnsignedTF PublicKey = 'True
    IsUnsignedTF (Hash a)  = 'True
    IsUnsignedTF Word32    = 'True
    IsUnsignedTF  _        = 'False

class ToTVM a ~ 'IntT => IsUnsigned a where
instance (IsUnsignedTF a ~ 'True, ToTVM a ~ 'IntT) => IsUnsigned a

-- Dictionary stuff below.
-- Type family needed to determin key size of datatype
-- It's needed to perform operations under dictionary.

type family BitSize k :: Nat
type instance BitSize Signature = 512
type instance BitSize PublicKey = 256
type instance BitSize (Hash a) = 256

bitSize :: forall k . KnownNat (BitSize k) => Bits
bitSize = fromIntegral $ natVal @(BitSize k) @Proxy Proxy

-- Instructions over :->

drop :: ProhibitMaybe (ToTVM a) => a & s :-> s
drop = I DROP

dup :: forall a s . ProhibitMaybe (ToTVM a) => a & s :-> a & a & s
dup = I (PUSH (Proxy @0))

swap :: ProhibitMaybes '[ToTVM a, ToTVM b] => a & b & s :-> b & a & s
swap = I SWAP

push :: forall (i :: Nat) s .
    ( ProhibitMaybes (Take (i + 1) (ToTVMs s))
    , PushTF i (ToTVMs s) ~ ToTVMs (PushTF i s)
    , KnownNat i
    )
    => s :-> PushTF i s
push = I (PUSH (Proxy @i))

pushInt :: (Integral a, ToTVM a ~ 'IntT) => a -> (s :-> a & s)
pushInt = I . PUSHINT . toInteger

-- Unit represents empty cell
unit :: s :-> () & s
unit = I $ NEWC `Seq` ENDC `Seq` CTOS

true :: s :-> Bool & s
true = I TRUE

false :: s :-> Bool & s
false = I FALSE

pop :: forall (i :: Nat) s .
    ( ProhibitMaybes (Take (i + 1) (ToTVMs s))
    , PopTF i (ToTVMs s) ~ ToTVMs (PopTF i s)
    , KnownNat i
    )
    => s :-> PopTF i s
pop = I (POP (Proxy @i))

rollRev
    :: forall (n :: Nat) s .
    ( ProhibitMaybes (Take n (ToTVMs s)), 1 <= n
    , RollRevTF n (ToTVMs s) ~ ToTVMs (RollRevTF n s)
    , KnownNat n
    )
    => s :-> RollRevTF n s
rollRev = I (ROLLREV (Proxy @n))

roll
    :: forall (n :: Nat) s .
    ( ProhibitMaybes (Take n (ToTVMs s)), 1 <= n
    , RollTF n (ToTVMs s) ~ ToTVMs (RollTF n s)
    , KnownNat n
    )
    => s :-> RollTF n s
roll = I (ROLL (Proxy @n))

-- equal to ROLLREV (i + 1)
moveOnTop
    :: forall (i :: Nat) s .
    ( ProhibitMaybes (Take (i + 1) (ToTVMs s)), 1 <= i + 1
    , RollRevTF (i + 1) (ToTVMs s) ~ ToTVMs (RollRevTF (i + 1) s)
    , KnownNat (i + 1)
    )
    => s :-> RollRevTF (i + 1) s
moveOnTop = rollRev @(i + 1)

reversePrefix
    :: forall (n :: Nat) s .
    ( ProhibitMaybes (Take n (ToTVMs s)), 2 <= n
    , Reverse (Take n (ToTVMs s)) ~ ToTVMs (Reverse (Take n s))
    , KnownNat n
    )
    => s :-> Reverse (Take n s)
reversePrefix = I (REVERSE_PREFIX (Proxy @n))

pushRoot :: forall a s . s :-> (Cell a & s)
pushRoot = I PUSHROOT

popRoot :: forall a s . (Cell a & s) :-> s
popRoot = I POPROOT

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

dataHash :: forall a x s . ToTVM x ~ 'SliceT => x & s :-> Hash a & s
dataHash = I SHA256U

cellHash :: Cell a & s :-> Hash a & s
cellHash = I HASHCU

chkSignS :: PublicKey & Signature & Slice & s :-> Bool & s
chkSignS = I CHKSIGNS

chkSignU :: PublicKey & Signature & Hash a & s :-> Bool & s
chkSignU = I CHKSIGNU

-- if statements
ifJust
    :: forall a s t . (ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s))
    => (a ++ s :-> t) -> (s :-> t) -> (Mb a & s :-> t)
ifJust (I t) (I f) = I (IF_JUST t f)

ifNothing
    :: forall a s t . (ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s))
    => (s :-> t) -> (a ++ s :-> t)  -> (Mb a & s :-> t)
ifNothing (I f) (I t) = I (IF_JUST t f)

fmapMaybe
    :: forall a b s .
    ( ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s)
    , ToTVMs b ++ ToTVMs s ~ ToTVMs (b ++ s))
    => (a ++ s :-> b ++ s) -> (Mb a & s :-> Mb b & s)
fmapMaybe (I f) = I (FMAP_MAYBE f)

just :: forall a s . ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s)
     => a ++ s :-> Mb a & s
just = I JUST

nothing :: forall a s . s :-> Mb a & s
nothing = I NOTHING

ifElse  :: (s :-> t) -> (s :-> t)  -> (Bool & s :-> t)
ifElse (I t) (I f) = I (IFELSE t f)

-- | Predicate for @if ... then .. else ...@ construction,
-- defines a kind of operation applied to the top elements of the current stack.
data Condition st arg argl argr where
    Holds     :: Condition s (Bool ': s) s s
    NotHolds  :: Condition s (Bool ': s) s s
    IsJust    :: (ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s)) => Condition s (Mb a ': s) (a ++ s) s
    IsNothing :: (ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s)) => Condition s (Mb a ': s) s (a ++ s)

    IsEq :: ToTVM a ~ 'IntT => Condition s (a ': a ': s) s s
    -- IsNeq :: IfCmpXConstraints a Neq => Condition s (a ': a ': s) s s
    -- IsLt :: IfCmpXConstraints a Lt => Condition s (a ': a ': s) s s
    -- IsGt :: IfCmpXConstraints a Gt => Condition s (a ': a ': s) s s
    IsLe :: ToTVM a ~ 'IntT => Condition s (a ': a ': s) s s
    IsGe :: ToTVM a ~ 'IntT=> Condition s (a ': a ': s) s s

-- | Defines semantics of @if ... then ... else ...@ construction.
ifThenElse
    :: Condition st arg argl argr
    -> (argl :-> o) -> (argr :-> o) -> (arg :-> o)
ifThenElse = \case
    Holds -> ifElse
    NotHolds -> flip ifElse
    IsJust-> ifJust
    IsNothing -> flip ifJust

    IsEq -> \l r -> equalInt >> ifElse l r
    IsLe -> \l r -> leqInt >> ifElse l r
    IsGe -> \l r -> geqInt >> ifElse l r

while :: (s :-> Bool & s) -> (s :-> s) -> (s :-> s)
while (I st) (I body) = I (WHILE st body)

throwIf :: (Enum e, Exception e) => e -> (Bool & s :-> s)
throwIf = I . THROWIF

throwIfNot :: (Enum e, Exception e) => e -> (Bool & s :-> s)
throwIfNot = I . THROWIFNOT

-- Application specific instructions
now :: s :-> Timestamp & s
now = I NOW

sendRawMsg :: Word32 & Cell MessageObject & s :-> s
sendRawMsg = I SENDRAWMSG

-- Auxiliary DSL instructions
stacktype :: forall s . s :-> s
stacktype = I Ignore

stacktype' :: forall a s . (a ++ s) :-> (a ++ s)
stacktype' = I Ignore

cast :: forall a b s . a & s :-> b & s
cast = I Ignore

ignore :: forall a s . a & s :-> a & s
ignore = I Ignore

comment :: Text -> a :-> a
comment = I . Comment