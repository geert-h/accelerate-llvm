{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Accelerate.LLVM.Metal.Kernel
  ( MetalKernel(..)
  , MetalKernelMetadata(..)
  , kernelArgLayout
  , alignArgumentOffset
  ) where

import Control.DeepSeq (rnf)
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int32)
import Data.String (fromString)

import Data.Array.Accelerate.Analysis.Hash.Operation (hashOperation)
import Data.Array.Accelerate.AST.Idx (Idx)
import Data.Array.Accelerate.AST.Kernel (IsKernel(..), KernelArgR(..), OpenKernelFun(..))
import Data.Array.Accelerate.Backend (NFData'(..))
import Data.Array.Accelerate.Pretty.Schedule (PrettyKernel(..), PrettyKernelStyle(..))
import Data.Array.Accelerate.Error (internalError)
import Data.Array.Accelerate.Type (ScalarType(..), SingleDict(..), singleDict)
import Data.Array.Accelerate.LLVM.CodeGen.Environment (sizeOfEnv)

import Data.Array.Accelerate.LLVM.Metal.CodeGen (MetalCode(..), codegen)
import Data.Array.Accelerate.LLVM.Metal.Compile (withCompiledModule)
import Data.Array.Accelerate.LLVM.Metal.State (evalMetal, defaultTarget)
import Data.Array.Accelerate.LLVM.Compile.Cache (UID)
import qualified Data.Array.Accelerate.LLVM.Metal.Link as Link
import Data.Array.Accelerate.LLVM.Metal.Link.Object (KernelObject)
import Data.Array.Accelerate.LLVM.Metal.Operation (MetalOp)
import Data.Array.Accelerate.Array.Buffer (Buffer)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Foreign.Storable (alignment, sizeOf)
import Control.Monad.IO.Class (liftIO)
import Data.Array.Accelerate.LLVM.Metal.Target (metalContext)

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

-- Kind of a misleading name, it stores the size and alignment of the kernel's argument buffer
-- I propose to rename it to ArgumentBufferLayout or something along those lines
data MetalKernelMetadata f = MetalKernelMetadata
  { kernelArgSize       :: !Int
  , kernelArgsAlignment :: !Int
  }
  deriving Show

instance NFData' MetalKernelMetadata where
  rnf' (MetalKernelMetadata bytes align) = rnf bytes `seq` rnf align

-- Simple aligment offset calculator
alignArgumentOffset :: Int -> Int -> Int
alignArgumentOffset off al = off + ((-off) `mod` al)

kernelArgLayout :: forall t r. KernelArgR t r -> (Int, Int)
kernelArgLayout (KernelArgRbuffer _ _)
  | pointerLayout == addressLayout = addressLayout
  | otherwise = internalError "Metal GPU addresses must match the pointer layout used by sizeOfEnv"
  where
    pointerLayout = (alignment (undefined :: Ptr ()), sizeOf (undefined :: Ptr ()))
    addressLayout = (alignment (undefined :: Word64), sizeOf (undefined :: Word64))
kernelArgLayout (KernelArgRscalar (SingleScalarType tp))
  | SingleDict <- singleDict tp = (alignment (undefined :: r), sizeOf (undefined :: r))
kernelArgLayout (KernelArgRscalar (VectorScalarType _)) = internalError "Metal vector-valued kernel arguments not implemented"

kernelArgumentsAlignment :: OpenKernelFun MetalKernel env f -> Int
kernelArgumentsAlignment (KernelFunBody _) = 1
kernelArgumentsAlignment (KernelFunLam arg rest) = max (fst (kernelArgLayout arg)) (kernelArgumentsAlignment rest)

instance IsKernel MetalKernel where
  type KernelOperation MetalKernel = MetalOp
  type KernelMetadata  MetalKernel = MetalKernelMetadata

  kernelMetadata func =
    MetalKernelMetadata
    { kernelArgSize = alignArgumentOffset (sizeOfEnv func) (kernelArgumentsAlignment func)
    , kernelArgsAlignment = kernelArgumentsAlignment func
    }

  compileKernel parameterTypes cluster args =
    unsafePerformIO $ evalMetal defaultTarget $ do
      let uid = hashOperation cluster args

      generated <- codegen ("generate_" ++ show uid) parameterTypes cluster args

      executable <- liftIO $ withCompiledModule (metalCodeSource generated) $ \path ->
        Link.loadKernel (metalContext defaultTarget) path (metalCodeName generated)

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
