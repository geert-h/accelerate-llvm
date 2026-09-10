{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.Array.Accelerate.LLVM.Metal.Execute.Marshal
  ( baseToValues
  , MarshalledKernel(..)
  , marshalKernel
  ) where

import Control.Concurrent.MVar (newMVar, readMVar)
import Data.IORef (readIORef, writeIORef)
import Foreign.ForeignPtr (withForeignPtr)

import Data.Array.Accelerate.Array.Buffer (Buffer(..), newBuffer, unsafeFreezeBuffer)
import Data.Array.Accelerate.Representation.Elt (scalarTypeSize)
import Data.Array.Accelerate.Representation.Ground (GroundR(..))
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.AST.Kernel (OpenKernelFun(..), KernelFun, KernelArgR(..))
import Data.Array.Accelerate.AST.Operation (PreArgs(..))
import Data.Array.Accelerate.AST.Schedule.Uniform (BaseR(..), BasesR, Signal(..), SignalResolver(..), Ref(..), OutputRef(..), SArg(..), SArgs)
import Data.Array.Accelerate.AST.Var (Var(..))

import Data.Array.Accelerate.LLVM.Metal.Array.Data (bufferBytes)
import qualified Data.Array.Accelerate.LLVM.Metal.Array.Prim as Buffer
import Data.Array.Accelerate.LLVM.Metal.Execute.Environment
import Data.Array.Accelerate.LLVM.Metal.Kernel (MetalKernel)

baseToValues :: BasesR t -> t -> IO (Distribute Value t)
baseToValues TupRunit _ = pure ()
-- scalar input, wait for the host value and put it in an internal reference
baseToValues (TupRpair (TupRsingle BaseRsignal) (TupRsingle (BaseRref (GroundRscalar tp)))) (Signal ready, Ref input) = do
  readMVar ready
  value <- readIORef input
  ref <- newMVar (ValueScalar tp value)
  pure (ValueSignal ready, ValueRef ref)
-- scalar output, writing the scheduled reference updates the host reference
baseToValues (TupRpair (TupRsingle BaseRsignalResolver) (TupRsingle (BaseRrefWrite (GroundRscalar _)))) (SignalResolver ready, OutputRef output) =
  pure
    (ValueSignalResolver ready
    , ValueOutputRef $ \case
        ValueScalar _ x -> writeIORef output x
        _ -> fail "accelerate-llvm-metal: expected scalar output"
    )
-- buffer output. allocate a proper Accelerate host buffer and copy into it
baseToValues (TupRpair (TupRsingle BaseRsignalResolver) (TupRsingle (BaseRrefWrite (GroundRbuffer tp)))) (SignalResolver ready, OutputRef output) =
  pure
    (ValueSignalResolver ready
    , ValueOutputRef $ \case
        ValueBuffer _ deviceBuffer -> do
          let bytes = bufferBytes deviceBuffer
              elementBytes = scalarTypeSize tp
              count = bytes `div` elementBytes
          if bytes `mod` elementBytes /= 0
            then fail "accelerate-llvm-metal: invalid output buffer size"
            else do
              mutable <- newBuffer tp count
              let host@(Buffer pointer) = unsafeFreezeBuffer mutable
              withForeignPtr pointer $ \destination ->
                Buffer.copyToHost deviceBuffer bytes destination
              writeIORef output host
        _ -> fail "accelerate-llvm-metal: expected buffer output"
    )
-- match the signal/reference pairs above before this general tuple case
baseToValues (TupRpair a b) (x, y) = do
  x' <- baseToValues a x
  y' <- baseToValues b y
  pure (x', y')
baseToValues _ _ = fail "accelerate-llvm-metal: unsupported function argument"

data MarshalledKernel where
  MarshalledKernel ::  MetalKernel env -> Gamma env -> MarshalledKernel

marshalKernel :: forall outer f. Gamma outer -> KernelFun MetalKernel f -> SArgs outer f -> IO MarshalledKernel
marshalKernel outer = go Empty
  where
    go :: forall inner remaining. Gamma inner -> OpenKernelFun MetalKernel inner remaining -> SArgs outer remaining -> IO MarshalledKernel
    go inner (KernelFunBody kernel) ArgsNil = pure (MarshalledKernel kernel inner)
    go inner (KernelFunLam (KernelArgRscalar tp) rest) (SArgScalar (Var _ idx) :>: args) =
      case prj' idx outer of
        ValueScalar _ value ->
          go (Push inner (ValueScalar tp value)) rest args
        _ -> fail "accelerate-llvm-metal: expected scalar kernel argument"
    go inner (KernelFunLam (KernelArgRbuffer _ tp) rest) (SArgBuffer _ (Var _ idx) :>: args) =
      case prj' idx outer of
        ValueBuffer _ buffer ->
          go (Push inner (ValueBuffer tp buffer)) rest args
        _ -> fail "accelerate-llvm-metal: expected buffer kernel argument"
