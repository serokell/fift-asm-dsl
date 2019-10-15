-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE PolyKinds #-}

module FiftAsm.Program
    ( UntypedDeclaration
    , decl
    , declMethod

    , Program (..)
    , declProgram
    ) where

import Prelude hiding (foldMap)

import Data.Bits ((.|.))
import Data.ByteString.Char8 (pack)
import Data.Foldable (foldMap)
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

declMethod :: inp :-> out -> (Maybe Word16, UntypedDeclaration)
declMethod = (Nothing,) . UntypedDeclaration

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

declareProc :: String -> Builder
declareProc name =
    "DECLPROC " <> build name <> "\n"

declareMethod :: String -> Maybe Word16 -> Builder
declareMethod name mId =
    build methodId <> " DECLMETHOD " <> build name <> "\n"
  where
    methodId :: Int
    methodId = maybe autoId fromIntegral mId
    autoId = fromIntegral (crc16 . pack $ name) .|. 0x10000


buildSubroutine :: String -> Subroutine -> Builder
buildSubroutine name (Subroutine instr) =
    build name <> " PROC:<{\n" <>
    indentF indentation (buildInstr instr) <>
    "}>\n"

buildUntypedDeclaration :: String -> UntypedDeclaration -> Builder
buildUntypedDeclaration name (UntypedDeclaration (I instr _)) =
    buildSubroutine name (Subroutine instr)


instance Buildable Program where
    build (Program { pProcedures, pMethods }) =
        "PROGRAM{\n" <>

        foldMap declareProc (M.keys subroutines) <>
        M.foldMapWithKey buildSubroutine subroutines <>

        M.foldMapWithKey
          (\n p -> declareProc n <> buildUntypedDeclaration n p) pProcedures <>

        M.foldMapWithKey
          (\n (i, m) -> declareMethod n i <> buildUntypedDeclaration n m) pMethods <>

        "}END>c"
      where
        subroutines
          = unionSubroutines pProcedures
         <> unionSubroutines (fmap snd pMethods)

unionSubroutines :: Foldable f => f UntypedDeclaration -> Map String Subroutine
unionSubroutines = foldMap getSubroutines
  where
    getSubroutines (UntypedDeclaration (I _ rs)) = rs
