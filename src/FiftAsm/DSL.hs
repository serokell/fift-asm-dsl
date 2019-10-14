{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PolyKinds #-}

module FiftAsm.DSL
       ( (:->) (..)
       , type (&)
       , (>>)
       , ToTVM
       , ToTVMs
       , mkI

       , Subroutine (..)
       , viaSubroutine

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
       , nip
       , moveOnTop
       , rollRev
       , roll
       , reversePrefix
       , xchg
       , xchg'
       , xchg2
       , xcpu
       , xc2pu

       , pushRoot
       , popRoot

       , ldSliceX
       , stSlice
       , ld32Unsigned
       , pld32Unsigned
       , st32Unsigned
       , endS
       , cToS
       , srefs

       , inc
       , equalInt
       , geqInt
       , leqInt
       , greaterInt

       , dataHash
       , cellHash
       , sliceHash
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
       , throw
       , throwIf
       , throwIfNot

       , now
       , sendRawMsg
       , accept

       , stacktype
       , stacktype'
       , cast
       , ignore
       , comment

       , pair
       , unpair
       ) where

import Prelude

import qualified Data.Kind as Kind
import GHC.TypeLits (type (+), type (<=))
import GHC.TypeLits.Extra (Max)
import Data.Typeable (typeRep, typeRepFingerprint)
import qualified Data.Map as M

import FiftAsm.Instr
import FiftAsm.Types
import Util

type family ToTVM (t :: *) :: T

type family ToTVMs xs where
    ToTVMs '[] = '[]
    ToTVMs (x ': xs) = ToTVM x ': ToTVMs xs

data Subroutine where
  Subroutine :: Instr inp out -> Subroutine

deriving instance Show Subroutine

-- | Alias for instruction which hides inner types representation via 'T'.
data (inp :: [Kind.Type]) :-> (out :: [Kind.Type]) =
    I { instr :: Instr (ToTVMs inp) (ToTVMs out), subRoutines :: Map String Subroutine }
    deriving (Show)
infixr 1 :->

viaSubroutine
    :: forall inp out s.
      (Typeable inp, Typeable out)
    => String
    -> inp :-> out
    -> (inp ++ s) :-> (out ++ s)
viaSubroutine funPrefix (I action rs) =
    I (CALL funName) (M.insert funName (Subroutine action) rs)
  where
    tr p = take 6 $ show (typeRepFingerprint (typeRep p))
    funName = funPrefix <> "_" <> tr (Proxy @inp) <> "_" <> tr (Proxy @out)

(>>) :: (a :-> b) -> (b :-> c) -> (a :-> c)
(>>) (I l lR) (I r rR) = I (l `Seq` r) (lR <> rR)

mkI :: Instr (ToTVMs inp) (ToTVMs out) -> inp :-> out
mkI = flip I mempty

type (&) (a :: Kind.Type) (b :: [Kind.Type]) = a ': b
infixr 5 &

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
type instance ToTVM Word8     = 'IntT
type instance ToTVM Word32    = 'IntT
type instance ToTVM Timestamp = 'IntT
type instance ToTVM Bits      = 'IntT
type instance ToTVM Bool      = 'IntT
type instance ToTVM Integer   = 'IntT
type instance ToTVM Natural   = 'IntT
type instance ToTVM (Cell a)  = 'CellT
type instance ToTVM Builder   = 'BuilderT
type instance ToTVM ()        = 'SliceT
type instance ToTVM ((,) a b) = 'TupleT '[ToTVM a, ToTVM b]
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
drop = mkI DROP

dup :: forall a s . ProhibitMaybe (ToTVM a) => a & s :-> a & a & s
dup = mkI (PUSH (Proxy @0))

swap :: ProhibitMaybes '[ToTVM a, ToTVM b] => a & b & s :-> b & a & s
swap = mkI SWAP

push :: forall (i :: Nat) s .
    ( ProhibitMaybes (Take (i + 1) (ToTVMs s))
    , PushTF i (ToTVMs s) ~ ToTVMs (PushTF i s)
    , KnownNat i
    )
    => s :-> PushTF i s
push = mkI (PUSH (Proxy @i))

pushInt :: (Integral a, ToTVM a ~ 'IntT) => a -> (s :-> a & s)
pushInt = mkI . PUSHINT . toInteger

unpair :: (a, b) & s :-> a & b & s
unpair = mkI UNPAIR

pair :: a & b & s :-> (a, b) & s
pair = mkI PAIR

-- Unit represents empty cell
unit :: s :-> () & s
unit = mkI $ NEWC `Seq` ENDC `Seq` CTOS

true :: s :-> Bool & s
true = mkI TRUE

false :: s :-> Bool & s
false = mkI FALSE

pop :: forall (i :: Nat) s .
    ( ProhibitMaybes (Take (i + 1) (ToTVMs s))
    , PopTF i (ToTVMs s) ~ ToTVMs (PopTF i s)
    , KnownNat i
    )
    => s :-> PopTF i s
pop = mkI (POP (Proxy @i))

nip ::
    ( ProhibitMaybeTF (ToTVM a)
    , ProhibitMaybeTF (ToTVM b)
    )
    => a & b & s :-> a & s
nip = pop @1

rollRev
    :: forall (n :: Nat) s .
    ( ProhibitMaybes (Take n (ToTVMs s)), 1 <= n
    , RollRevTF n (ToTVMs s) ~ ToTVMs (RollRevTF n s)
    , KnownNat n
    )
    => s :-> RollRevTF n s
rollRev = mkI (ROLLREV (Proxy @n))

-- | Check @2 roll ~ rot@
_rollTest
  :: ProhibitMaybes (ToTVMs '[x, y, z])
  => '[z, y, x] :-> '[x, z, y]
_rollTest = roll @2

-- | Check @2 rollRev ~ rot rot@
_rollRevTest
  :: ProhibitMaybes (ToTVMs '[x, y, z])
  => '[z, y, x] :-> '[y, x, z]
_rollRevTest = rollRev @2

roll
    :: forall (n :: Nat) s .
    ( ProhibitMaybes (Take n (ToTVMs s)), 1 <= n
    , RollTF n (ToTVMs s) ~ ToTVMs (RollTF n s)
    , KnownNat n
    )
    => s :-> RollTF n s
roll = mkI (ROLL (Proxy @n))

-- equal to ROLL i
moveOnTop
    :: forall (i :: Nat) s .
    ( ProhibitMaybes (Take i (ToTVMs s)), 1 <= i
    , RollTF i (ToTVMs s) ~ ToTVMs (RollTF i s)
    , KnownNat i
    )
    => s :-> RollTF i s
moveOnTop = roll @i

reversePrefix
    :: forall (n :: Nat) s s' .
    ( ProhibitMaybes (Take n (ToTVMs s)), 2 <= n
    , KnownNat n
    , s' ~ (Reverse (Take n s) ++ Drop n s)
    , (Reverse (Take n (ToTVMs s)) ++ Drop n (ToTVMs s)) ~ ToTVMs s'
    )
    => s :-> s'
reversePrefix = mkI (REVERSE_PREFIX (Proxy @n))

xchg
  :: forall (i :: Nat) s .
  ( ProhibitMaybes (Take i (ToTVMs s)), 1 <= i, KnownNat i
  , XchgTF 0 i (ToTVMs s) ~ ToTVMs (XchgTF 0 i s)
  )
  => s :-> XchgTF 0 i s
xchg = mkI (XCHG (Proxy @i))

xchg'
  :: forall (i :: Nat) (j :: Nat) s .
  ( ProhibitMaybes (Take (Max i j) (ToTVMs s)), KnownNat i, KnownNat j
  , XchgTF i j (ToTVMs s) ~ ToTVMs (XchgTF i j s)
  )
  => s :-> XchgTF i j s
xchg' = mkI (XCHG' (Proxy @i) (Proxy @j))

xchg2
  :: forall (i :: Nat) (j :: Nat) s .
  ( ProhibitMaybes (Take (Max i j) (ToTVMs s)), KnownNat i, KnownNat j
  , Xchg2TF i j (ToTVMs s) ~ ToTVMs (Xchg2TF i j s)
  )
  => s :-> Xchg2TF i j s
xchg2 = mkI (XCHG2 (Proxy @i) (Proxy @j))

xcpu
  :: forall (i :: Nat) (j :: Nat) s .
  ( ProhibitMaybes (Take (Max i j) (ToTVMs s))
  , PushTF j (XchgTF 0 i (ToTVMs s)) ~ ToTVMs (PushTF j (XchgTF 0 i s))
  , KnownNat i, KnownNat j)
  => s :-> PushTF j (XchgTF 0 i s)
xcpu = mkI (XCPU (Proxy @i) (Proxy @j))

xc2pu
  :: forall (i :: Nat) (j :: Nat) (k :: Nat) s .
  ( ProhibitMaybes (Take (Max (Max i j) k) (ToTVMs s))
  , PushTF k (Xchg2TF i j (ToTVMs s)) ~ ToTVMs (PushTF k (Xchg2TF i j s))
  , KnownNat i, KnownNat j, KnownNat k)
  => s :-> PushTF k (Xchg2TF i j s)
xc2pu = mkI (XC2PU (Proxy @i) (Proxy @j) (Proxy @k))


pushRoot :: forall a s . s :-> (Cell a & s)
pushRoot = mkI PUSHROOT

popRoot :: forall a s . (Cell a & s) :-> s
popRoot = mkI POPROOT

ldSliceX :: forall a s . ToTVM a ~ 'SliceT => Bits & Slice & s :-> Slice & a & s
ldSliceX = mkI LDSLICEX

stSlice :: forall a s . ToTVM a ~ 'SliceT => Builder & a & s :-> Builder & s
stSlice = mkI STSLICE

ld32Unsigned :: forall a s . ToTVM a ~ 'IntT => Slice & s :-> Slice & a & s
ld32Unsigned = mkI (LDU 31)

pld32Unsigned :: forall a s . ToTVM a ~ 'IntT => Slice & s :-> a & s
pld32Unsigned = mkI (PLDU 31)

st32Unsigned :: forall a s . ToTVM a ~ 'IntT => Builder & a & s :-> Builder & s
st32Unsigned = mkI (STU 31)

endS :: Slice & s :-> s
endS = mkI ENDS

cToS :: forall a s . Cell a & s :-> Slice & s
cToS = mkI CTOS

srefs :: forall a s . ToTVM a ~ 'IntT => Slice & s :-> a & s
srefs = mkI SREFS

inc :: ToTVM a ~ 'IntT => a & s :-> a & s
inc = mkI INC

equalInt :: ToTVM a ~ 'IntT => a & a & s :-> Bool & s
equalInt = mkI EQUAL

geqInt :: ToTVM a ~ 'IntT => a & a & s :-> Bool & s
geqInt = mkI GEQ

greaterInt :: ToTVM a ~ 'IntT => a & a & s :-> Bool & s
greaterInt = mkI GREATER

leqInt :: ToTVM a ~ 'IntT => a & a & s :-> Bool & s
leqInt = mkI LEQ

dataHash :: forall a x s . ToTVM x ~ 'SliceT => x & s :-> Hash a & s
dataHash = mkI SHA256U

cellHash :: Cell a & s :-> Hash a & s
cellHash = mkI HASHCU

sliceHash :: Slice & s :-> Hash Slice & s
sliceHash = mkI HASHSU

chkSignS :: PublicKey & Signature & Slice & s :-> Bool & s
chkSignS = mkI CHKSIGNS

chkSignU :: PublicKey & Signature & Hash a & s :-> Bool & s
chkSignU = mkI CHKSIGNU

-- if statements
ifJust
    :: forall a s t . (ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s))
    => (a ++ s :-> t) -> (s :-> t) -> (Mb a & s :-> t)
ifJust (I t tR) (I f fR) = I (IF_JUST t f) (tR <> fR)

ifNothing
    :: forall a s t . (ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s))
    => (s :-> t) -> (a ++ s :-> t)  -> (Mb a & s :-> t)
ifNothing (I f fR) (I t tR) = I (IF_JUST t f) (tR <> fR)

fmapMaybe
    :: forall a b s .
    ( ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s)
    , ToTVMs b ++ ToTVMs s ~ ToTVMs (b ++ s))
    => (a ++ s :-> b ++ s) -> (Mb a & s :-> Mb b & s)
fmapMaybe (I f r) = I (FMAP_MAYBE f) r

just :: forall a s . ToTVMs a ++ ToTVMs s ~ ToTVMs (a ++ s)
     => a ++ s :-> Mb a & s
just = mkI JUST

nothing :: forall a s . s :-> Mb a & s
nothing = mkI NOTHING

ifElse  :: (s :-> t) -> (s :-> t)  -> (Bool & s :-> t)
ifElse (I t tR) (I f fR) = I (IFELSE t f) (tR <> fR)

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
    IsGt :: ToTVM a ~ 'IntT => Condition s (a ': a ': s) s s
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
    IsGt -> \l r -> greaterInt >> ifElse l r

while :: (s :-> Bool & s) -> (s :-> s) -> (s :-> s)
while (I st stR) (I body bodyR) = I (WHILE st body) (stR <> bodyR)

throw :: (Enum e, Exception e) => e -> (s :-> t)
throw = mkI . THROW

throwIf :: (Enum e, Exception e) => e -> (Bool & s :-> s)
throwIf = mkI . THROWIF

throwIfNot :: (Enum e, Exception e) => e -> (Bool & s :-> s)
throwIfNot = mkI . THROWIFNOT

-- Application specific instructions
now :: s :-> Timestamp & s
now = mkI NOW

sendRawMsg :: Word8 & Cell MessageObject & s :-> s
sendRawMsg = mkI SENDRAWMSG

accept :: s :-> s
accept = mkI ACCEPT

-- Auxiliary DSL instructions
stacktype :: forall s . s :-> s
stacktype = mkI Ignore

stacktype' :: forall a s . (a ++ s) :-> (a ++ s)
stacktype' = mkI Ignore

cast :: forall a b s . a & s :-> b & s
cast = mkI Ignore

ignore :: forall a s . a & s :-> a & s
ignore = mkI Ignore

comment :: Text -> a :-> a
comment = mkI . Comment
