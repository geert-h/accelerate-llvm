{-# LANGUAGE OverloadedStrings #-}

module Data.Array.Accelerate.LLVM.Metal.Target
  ( Metal(..)
  , metalTargetTriple
  , metalDataLayout
  ) where

import Data.Array.Accelerate.LLVM.Metal.Context (Context)
import Data.Array.Accelerate.LLVM.Target (Target(..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty.AST as LLVM
import Data.Array.Accelerate.Error (internalError)

newtype Metal = Metal
  { metalContext :: Context
  }

instance Target Metal where
  targetTriple     = Just metalTargetTriple
  targetDataLayout = Just metalDataLayout

metalTargetTriple :: ShortByteString
metalTargetTriple = "air64_v28-apple-macosx26.0.0"

metalDataLayoutString :: String
metalDataLayoutString = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:256-v256:256:256-v512:512:512-v1024:1024:1024-n8:16:32"

metalDataLayout :: LLVM.DataLayout
metalDataLayout = case LLVM.parseDataLayout metalDataLayoutString of
 Just l -> l
 Nothing -> internalError "error while parsing metal data layout"
  
