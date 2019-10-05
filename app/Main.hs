module Main where

import Fmt

import MultiSig
import FiftAsm

main :: IO ()
main = do
    putText (fmt (build (AsProgram recvExternal)))