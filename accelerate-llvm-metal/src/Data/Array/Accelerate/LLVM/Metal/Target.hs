module Data.Array.Accelerate.LLVM.Metal.Target
  ( Metal(..)
  ) where

import Data.Array.Accelerate.LLVM.Metal.Context (Context)

newtype Metal = Metal
  { metalContext :: Context
  }
