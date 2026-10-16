{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Array.Accelerate.Test.NoFib
import Data.Array.Accelerate.LLVM.Metal
import Data.Array.Accelerate.LLVM.Metal.NoFib.RunQ

main :: IO ()
main = nofib runN test_runq
