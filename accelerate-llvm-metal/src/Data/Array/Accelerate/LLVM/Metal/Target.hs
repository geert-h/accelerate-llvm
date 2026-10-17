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
import Data.Array.Accelerate.LLVM.CodeGen.Intrinsic (Intrinsic (..))
import LLVM.AST.Type.Name
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

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
  
instance Intrinsic Metal where
  intrinsicForTarget = llvmMetalIntrinsic

-- TODO: These operations are incomplete
-- Not only the operations are (probably incomplete),
-- but also all the operations should be added for the different types apart from float.
-- For example the llvm operation fmax(a, b) should be one of the following cases based
-- on the type with which it is called:
-- - @air.fmax.f32(float, float)    for floating point
-- - @air.fmax.s.i32(i32, i32)      for signed 32-bit integers
-- - @air.fmax.u.i32(i32, i32)      for unsigned 32-bit integers
-- - others for i16, u16, i8, u8 (see the full list in chapter 2, Data types) of the language specs
-- n.b. these operations are (for now) generated with `-fno-fast-math`
--
-- The entire list which should be (mostly) added can be found in chapter 8, Numerical Compliance
-- https://developer.apple.com/metal/Metal-Shading-Language-Specification.pdf
llvmMetalIntrinsic :: HashMap ShortByteString Label
llvmMetalIntrinsic =
  let floating base rest
          = (base <> "f", Label ("air." <> base <> ".f32"))
          : rest
  in
  HashMap.fromList $ foldr floating []
    [ "max"
    , "min"
    , "fmax"
    , "fmin"
    , "sin"
    , "cos"
    , "tan"
    , "asin"
    , "acos"
    , "atan"
    , "asin"
    , "acos"
    , "atan"
    , "asinh"
    , "acosh"
    , "atanh"
    , "atan2"
    , "sqrt"
    , "powi"
    , "pow"
    , "exp"
    , "exp2"
    , "log"
    , "round"
    , "floor"
    , "abs"
    , "fabs"
    , "ceil"
    , "signum"
    ]
