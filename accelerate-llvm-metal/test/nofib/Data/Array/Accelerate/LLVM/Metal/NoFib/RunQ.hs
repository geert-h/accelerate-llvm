{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Array.Accelerate.LLVM.Metal.NoFib.RunQ where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Metal as Metal

import Test.Tasty
import Test.Tasty.HUnit


test_runq :: TestTree
test_runq =
  testGroup "runQ"
    [ {- testCase "simple" test_simple -} ]

