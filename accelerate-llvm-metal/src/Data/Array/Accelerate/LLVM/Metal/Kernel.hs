{-# LANGUAGE TypeFamilies      #-}

module Data.Array.Accelerate.LLVM.Metal.Kernel (
  MetalKernel(..),
) where

import Control.DeepSeq (rnf)
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int32)
import Data.String (fromString)

import Data.Array.Accelerate.Analysis.Hash.Operation (hashOperation)
import Data.Array.Accelerate.LLVM.State (unliftIOLLVM)
import Data.Array.Accelerate.AST.Idx (Idx)
import Data.Array.Accelerate.AST.Kernel (IsKernel(..), NoKernelMetadata)
import Data.Array.Accelerate.Backend (NFData'(..))
import Data.Array.Accelerate.Pretty.Schedule (PrettyKernel(..), PrettyKernelStyle(..))

import Data.Array.Accelerate.LLVM.Metal.CodeGen (MetalCode(..), codegen)
import Data.Array.Accelerate.LLVM.Metal.Compile (withCompiledModule)
import Data.Array.Accelerate.LLVM.Metal.State (evalMetal, defaultTarget)
import Data.Array.Accelerate.LLVM.Compile.Cache (UID)
import qualified Data.Array.Accelerate.LLVM.Metal.Link as Link
import Data.Array.Accelerate.LLVM.Metal.Link.Object (KernelObject)
import Data.Array.Accelerate.LLVM.Metal.Operation (MetalOp)
import Data.Array.Accelerate.Array.Buffer (Buffer)

data MetalKernel env = MetalKernel
  { kernelUID      :: !UID
  , kernelMain     :: !KernelObject
  , kernelElements :: ![Idx env Int]
  , kernelOutput   :: !(Idx env (Buffer Int32))
  , kernelName     :: !String
  }

instance NFData' MetalKernel where
  rnf' (MetalKernel uid executable elements output name) =
    uid `seq` executable `seq` rnf elements `seq` rnf output `seq` rnf name

instance IsKernel MetalKernel where
  type KernelOperation MetalKernel = MetalOp
  type KernelMetadata  MetalKernel = NoKernelMetadata

  compileKernel parameterTypes cluster args =
    unsafePerformIO $ evalMetal defaultTarget $ do
      let uid = hashOperation cluster args

      generated <- codegen ("generate_" ++ show uid) parameterTypes cluster args

      executable <- unliftIOLLVM $ \run -> withCompiledModule (metalCodeSource generated) $ \path ->
        run $ Link.link path (metalCodeName generated)

      pure MetalKernel
        { kernelUID      = uid
        , kernelMain     = executable
        , kernelElements = metalCodeElements generated
        , kernelOutput   = metalCodeOutput generated
        , kernelName     = metalCodeName generated
        }

  encodeKernel = Left . kernelUID

instance PrettyKernel MetalKernel where
  prettyKernel =
    PrettyKernelBody False $ \_ kernel ->
      fromString (kernelName kernel)
