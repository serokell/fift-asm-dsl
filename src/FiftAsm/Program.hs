{-# LANGUAGE PolyKinds #-}

module FiftAsm.Program
    ( UntypedDeclaration
    , decl

    , Program (..)
    , declProgram
    ) where

import Data.Bits ((.|.))
import Data.ByteString.Char8 (pack)
import Data.Map (Map)
import Data.Word (Word16)
import Fmt

import qualified Data.Map as M

import FiftAsm.Builder (buildInstr, indentation)
import FiftAsm.DSL ((:->) (..), Subroutine (..))
import Util.Crc16 (crc16)


data UntypedDeclaration
    = forall inp out. UntypedDeclaration (inp :-> out)

decl :: inp :-> out -> UntypedDeclaration
decl = UntypedDeclaration

data Program = Program
    { pProcedures :: Map String UntypedDeclaration
    , pMethods :: Map String (Maybe Word16, UntypedDeclaration)
    }

declProgram
    :: [(String, UntypedDeclaration)]
    -> [(String, (Maybe Word16, UntypedDeclaration))]
    -> Program
declProgram procedures methods =
    Program (M.fromList procedures) (M.fromList methods)


--
-- Builders
--

data NamedDeclaration = NamedDeclaration String UntypedDeclaration

instance Buildable NamedDeclaration where
    build (NamedDeclaration name (UntypedDeclaration (I instr rs))) =
        foldMap declProc (M.keys rs) <>
        buildProc name (Subroutine instr) <>
        M.foldMapWithKey buildProc rs

declProc :: String -> Builder
declProc name =
    "DECLPROC " <> build name <> "\n"

declMethod :: String -> Maybe Word16 -> Builder
declMethod name mId =
    build methodId <> " DECLMETHOD " <> build name <> "\n"
  where
    methodId :: Int
    methodId = maybe autoId fromIntegral mId
    autoId = fromIntegral (crc16 . pack $ name) .|. 0x10000

buildProc :: String -> Subroutine -> Builder
buildProc name (Subroutine instr) =
    build name <> " PROC:<{\n" <> indentF indentation (buildInstr instr) <> "}>\n"


instance Buildable Program where
    build (Program { pProcedures, pMethods }) =
      "PROGRAM{\n" <>
      M.foldMapWithKey
        (\n p -> declProc n <> build (NamedDeclaration n p)) pProcedures <>
      M.foldMapWithKey
        (\n (i, m) -> declMethod n i <> build (NamedDeclaration n m)) pMethods <>
      "}END>c"
