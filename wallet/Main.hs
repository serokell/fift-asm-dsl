-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Prelude
import Fmt

import FiftAsm

main :: IO ()
main = putText $ pretty $ declProgram procedures methods
  where
    procedures =
      [ ("recv_external", decl recvExternal)
      , ("recv_internal", decl recvInternal)
      ]
    methods =
      [ ("seqno", declMethod getSeqno)
      ]


data Storage = Storage
    { sCnt :: Word32
    , sPubKey :: PublicKey
    }

instance DecodeSlice Storage where
    type DecodeSliceFields Storage = [PublicKey, Word32]
    decodeFromSliceImpl = do
        decodeFromSliceImpl @Word32
        decodeFromSliceImpl @PublicKey

instance EncodeBuilder Storage where
    encodeToBuilder = do
        encodeToBuilder @Word32
        encodeToBuilder @PublicKey

data WalletError
    = SeqNoMismatch
    | SignatureMismatch
    deriving (Eq, Ord, Show, Generic)

instance Exception WalletError

instance Enum WalletError where
    toEnum 33 = SeqNoMismatch
    toEnum 34 = SignatureMismatch
    toEnum _ = error "Uknown MultiSigError id"

    fromEnum SeqNoMismatch = 33
    fromEnum SignatureMismatch = 34

recvInternal :: '[Slice] :-> '[]
recvInternal = drop

recvExternal :: '[Slice] :-> '[]
recvExternal = do
  decodeFromSlice @Signature
  dup
  preloadFromSlice @Word32
  stacktype @[Word32, Slice, Signature]
  -- cnt cs sign

  pushRoot
  decodeFromCell @Storage
  stacktype @[PublicKey, Word32, Word32, Slice, Signature]
  -- pk cnt' cnt cs sign

  xcpu @1 @2
  stacktype @[Word32, Word32, PublicKey, Word32, Slice, Signature]
  -- cnt cnt' pk cnt cs sign

  equalInt >> throwIfNot SeqNoMismatch

  push @2
  sliceHash
  stacktype @[Hash Slice, PublicKey, Word32, Slice, Signature]
  -- hash pk cnt cs sign

  xc2pu @0 @4 @4
  stacktype @[PublicKey, Signature, Hash Slice, Word32, Slice, PublicKey]
  -- pubk sign hash cnt cs pubk

  chkSignU
  stacktype @[Bool, Word32, Slice, PublicKey]
  -- ? cnt cs pubk

  throwIfNot SignatureMismatch
  accept

  swap
  decodeFromSlice @Word32
  nip

  dup
  srefs @Word8

  pushInt 0
  equalInt
  ifElse ignore $ do
    decodeFromSlice @Word8
    decodeFromSlice @(Cell MessageObject)
    stacktype @[Slice, Cell MessageObject, Word8, Word32, PublicKey]
    xchg @2
    sendRawMsg
    stacktype @[Slice, Word32, PublicKey]

  endS
  inc

  encodeToCell @Storage
  popRoot


getSeqno :: '[] :-> '[Word32]
getSeqno = do
  pushRoot
  cToS
  preloadFromSlice @Word32
