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
       , pushInt
       , ldSliceX
       , ldDict
       , lookupSet
       , lookupMap
       , cast
       ) where


import qualified Data.Kind as Kind

import FiftAsm.Instr
import FiftAsm.Types

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

pushRoot :: forall a s . s :-> (a & s)
pushRoot = I (PUSHROOT `Seq` Ignore)

drop :: a & s :-> s
drop = I DROP

dup :: forall a s . a & s :-> a & a & s
dup = I (PUSH @0)

endS :: Slice & s :-> s
endS = I ENDS

swap :: a & b & s :-> b & a & s
swap = I SWAP

pushInt :: (Integral a, ToTVM a ~ 'IntT) => a -> (s :-> a & s)
pushInt = I . PUSHINT . toInteger

ldSliceX :: forall a s . ToTVM a ~ 'SliceT => Bits & Slice & s :-> Slice & a & s
ldSliceX = I LDSLICEX

ldDict :: forall k v s . Slice & s :-> Slice & Dictionary k v & s
ldDict = I LDDICT

lookupSet :: DSet (Hash a) & s :-> Bool & s
lookupSet = error "not implemented yet"

lookupMap :: Dictionary (Hash a) b & s :-> Maybe b & s
lookupMap = error "not implemented yet"

-- ldRefRToS :: Slice & s :-> Slice & Slice & s
-- ldRefRToS = I LDREFRTOS

-- sChkBitsQ :: (Bits & Slice & s) :-> (Bool & s)
-- sChkBitsQ  = I SCHKBITSQ

cast :: forall a b s . a & s :-> b & s
cast = I Ignore