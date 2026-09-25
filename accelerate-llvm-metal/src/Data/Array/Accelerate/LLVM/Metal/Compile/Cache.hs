{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Array.Accelerate.LLVM.Metal.Compile.Cache
    ( module Data.Array.Accelerate.LLVM.Compile.Cache
    ) where

import Data.Array.Accelerate.LLVM.Compile.Cache
import Data.Array.Accelerate.LLVM.Metal.Target (Metal)

instance Persistent Metal where
  targetCacheTemplate = pure "metal/should-be-generated/kernel.metallib"

