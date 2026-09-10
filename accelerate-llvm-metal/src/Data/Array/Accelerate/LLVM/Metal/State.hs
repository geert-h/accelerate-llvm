module Data.Array.Accelerate.LLVM.Metal.State
  ( evalMetal
  , createTarget
  , createTargetFromContext
  , defaultTarget
  ) where

import System.IO.Unsafe (unsafePerformIO)

import Data.Array.Accelerate.LLVM.State (LLVM, evalLLVM)
import Data.Array.Accelerate.LLVM.Metal.Target (Metal(..))

import qualified Data.Array.Accelerate.LLVM.Metal.Context as Context

evalMetal :: Metal -> LLVM Metal a -> IO a 
evalMetal target computation =
  Context.withContext (metalContext target) $ \_ ->
    evalLLVM target computation

createTarget :: IO Metal
createTarget = Metal <$> Context.new

createTargetFromContext :: Context.Context -> IO Metal
createTargetFromContext = pure . Metal

{-# NOINLINE defaultTarget #-}
defaultTarget :: Metal
defaultTarget = unsafePerformIO createTarget
