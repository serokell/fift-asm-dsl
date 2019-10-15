-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module FiftAsm.Types
       ( T (..)
       ) where

data T
    = IntT
    | CellT
    | TupleT [T]
    | SliceT
    | BuilderT
    | NullT

    | MaybeT [T] -- represented as either True : x : y : z ... or False
    | DictT
