{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.Array.Accelerate.LLVM.Metal.Kernel
  ( MetalKernel(..)
  , MetalKernelMetadata(..)
  , kernelArgLayout
  , alignArgumentOffset
  ) where

import Control.DeepSeq (rnf)
import System.IO.Unsafe (unsafePerformIO)
import Data.String (fromString)

import Data.Array.Accelerate.Analysis.Hash.Operation (hashOperation)
import Data.Array.Accelerate.AST.Idx (Idx)
import Data.Array.Accelerate.AST.Kernel (IsKernel(..), KernelArgR(..), OpenKernelFun(..))
import Data.Array.Accelerate.Backend (NFData'(..))
import Data.Array.Accelerate.Error (internalError)
import Data.Array.Accelerate.Type (ScalarType(..), SingleDict(..), singleDict)
import Data.Array.Accelerate.LLVM.CodeGen.Environment (sizeOfEnv)

import Data.Array.Accelerate.LLVM.Metal.CodeGen (MetalCode(..), codegen)
import Data.Array.Accelerate.LLVM.Metal.Compile (compile)
import Data.Array.Accelerate.LLVM.Metal.State (evalMetal, defaultTarget)
import Data.Array.Accelerate.LLVM.Compile.Cache (UID)
import Data.Array.Accelerate.LLVM.Metal.Operation (MetalOp (..))
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Foreign.Storable (alignment, sizeOf)
import Data.Array.Accelerate.AST.Schedule (generateKernelNameAndDescription)
import Data.Array.Accelerate.AST.Exp
import Data.Array.Accelerate.LLVM.Metal.Link
import Data.Array.Accelerate.Pretty.Schedule

data MetalKernel env = MetalKernel
  { kernelUID      :: !UID
  , kernelMain     :: !KernelObject
  , kernelElements :: ![Idx env Int]
  , kernelDescDetail :: String
  , kernelDescBrief  :: String
  }

instance NFData' MetalKernel where
  rnf' (MetalKernel uid executable elements desc brief) =
    uid `seq` executable `seq` rnf elements `seq` rnf desc `seq` rnf brief

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

      generated <- codegen fullName parameterTypes cluster args
      obj <- compile uid (fromString fullName) (metalCodeWork generated)
      exec <- link obj

      pure MetalKernel
        { kernelUID        = uid
        , kernelMain       = exec
        , kernelElements   = metalCodeElements generated
        , kernelDescDetail = detail
        , kernelDescBrief  = brief
        }
      where
        fullName = name ++ "-" ++ show uid
        uid = hashOperation cluster args
        (name, detail, brief) = generateKernelNameAndDescription operationName cluster

  encodeKernel = Left . kernelUID

instance PrettyKernel MetalKernel where
  prettyKernel =
    PrettyKernelBody False $ \_ _kernel ->
      fromString "still in the works"

operationName :: MetalOp t -> (Int, String, String)
operationName = \case
  MetalMap               -> (2, "map", "maps")
  MetalBackpermute       -> (1, "backpermute", "backpermutes")
  MetalGenerate          -> (2, "generate", "generates")
  MetalPermute           -> (5, "permute", "permutes")
  MetalPermute'          -> (5, "permute", "permutes")
  MetalScan LeftToRight  -> (4, "scanl", "scanls")
  MetalScan RightToLeft  -> (4, "scanr", "scanrs")
  MetalScan1 LeftToRight -> (4, "scanl", "scanls")
  MetalScan1 RightToLeft -> (4, "scanr", "scanrs")
  MetalScan' LeftToRight -> (4, "scanl", "scanls")
  MetalScan' RightToLeft -> (4, "scanr", "scanrs")
  MetalFold              -> (3, "fold", "folds")
  MetalFold1             -> (3, "fold", "folds")

