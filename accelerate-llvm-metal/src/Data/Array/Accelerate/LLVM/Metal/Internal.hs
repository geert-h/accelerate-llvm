module Data.Array.Accelerate.LLVM.Metal.Internal
  ( MetalKernel(..)
  , Metal(..)
  , defaultTarget
  , withLoadedKernel
  ) where

import Data.Array.Accelerate.LLVM.Metal.Kernel
import Data.Array.Accelerate.LLVM.Metal.Target
import Data.Array.Accelerate.LLVM.Metal.State (defaultTarget)
import Data.Array.Accelerate.LLVM.Metal.Link (withLoadedKernel)
import Data.Array.Accelerate.LLVM.Metal.Execute ()
