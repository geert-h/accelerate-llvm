{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Array.Accelerate.LLVM.Metal.Execute.Marshal
  ( baseToValues
  , MarshalledKernel(..)
  , marshalKernel
  , withKernelArguments
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

import Data.Array.Accelerate.LLVM.Metal.Kernel
  ( MetalKernel
  , MetalKernelMetadata(..)
  , kernelArgLayout
  , alignArgumentOffset
  )
import qualified Data.Array.Accelerate.LLVM.Metal.Array.Prim as Buffer
import Data.Array.Accelerate.LLVM.Metal.Execute.Environment

import Control.Monad (when)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (pokeByteOff)

import Data.Array.Accelerate.Error (internalError)
import Data.Array.Accelerate.Type
  ( ScalarType(..), SingleDict(..), singleDict )

import Data.Array.Accelerate.LLVM.Metal.Context (Context)
import Data.Array.Accelerate.LLVM.Metal.FFI (RawBuffer)
import Data.Array.Accelerate.LLVM.Metal.Array.Data (MetalBuffer(..), withBuffer)

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

-- Allocates an argument buffer and fills it with the values for a kernel invocation.
withKernelArguments
  :: forall outer f a. Context
  -> MetalKernelMetadata f
  -> Gamma outer
  -> KernelFun MetalKernel f
  -> SArgs outer f
  -> (MetalBuffer -> [Ptr RawBuffer] -> IO a)
  -> IO a
withKernelArguments context metadata outer function actuals action =
  -- allocates the argument buffer
  -- Metal requires a nonzero allocation, even for an empty argument list
  Buffer.withNewBuffer context (max 1 bytes) $ \arguments -> Buffer.withBufferContents arguments $ \contents -> do
      -- TODO: Idk whether this is necessary but it zeroes the content of the buffer (also padding)
      -- for the generate case, it works with and without it, I'll just leave it for now just to be safe
      fillBytes contents 0 (bufferBytes arguments)
      go 0 [] function actuals arguments contents
    where
      go :: forall inner remaining.                    -- these changes between recursive call, so they need to be polymorphic
            Int                                        -- cursor: byte directly after the last packed field
         -> [Ptr RawBuffer]                            -- resources: Metal buffer handles referenced by already packed fields
         -> OpenKernelFun MetalKernel inner remaining  -- function: to be processed function
         -> SArgs outer remaining                      -- arguments: corresponding runtime variable references
         -> MetalBuffer
         -> Ptr ()
         -> IO a
      -- Base case when both lists are empty
      go cursor resources (KernelFunBody _) ArgsNil arguments _ = do
        -- check whether the alignment is actually correct
        when (alignArgumentOffset cursor (kernelArgsAlignment metadata) /= bytes) $ internalError "Metal argument layout disagrees with metadata"
        -- action gets the populated argument buffer and resource handles
        -- reverse restores resource order because buffer handles were prepended
        -- not that big of an issue with the generate case because there is only one resource
        action arguments (reverse resources)
      -- Match scalar declaration with an argument
      -- retain arg but also check the type
      go cursor resources (KernelFunLam arg@(KernelArgRscalar tp) rest) (SArgScalar (Var _ idx) :>: args) arguments contents =
        case tp of
          SingleScalarType single
            | SingleDict <- singleDict single ->
                case prj' idx outer of
                  ValueScalar _ value -> do                        -- look up the scalar value
                    (offset, next) <- place cursor arg             -- calculate how it should be aligned
                    pokeByteOff contents offset value              -- write value into arg buffer
                    go next resources rest args arguments contents -- go to next args
                  _ -> internalError "Expected a scalar kernel argument"
          VectorScalarType _ -> internalError "Metal vector-valued arguments are not supported"
      -- Packs buffer arguments
      -- empty buffers have no Metal allocation
      go cursor resources (KernelFunLam arg@(KernelArgRbuffer _ _) rest) (SArgBuffer _ (Var _ idx) :>: args) arguments contents =
        case prj' idx outer of
          ValueBuffer _ buffer -> do
            (offset, next) <- place cursor arg
            withBuffer buffer $ \raw ->                        -- raw is Objective-C Metal buffer handle
              Buffer.withBufferGPUAddress buffer $ \address -> -- address of buffer's storage on GPU
              do
                pokeByteOff contents offset address
                let resources' = if raw == nullPtr then resources else raw : resources -- writes address
                go next resources' rest args arguments contents -- go to next args
          _ -> internalError "Expected a buffer kernel argument"

      bytes = kernelArgSize metadata

      -- Example, if cursor is 4 and the next field requires eight-byte alignment:
      -- cursor = 4
      -- offset = 8
      -- width = 8
      -- next = 16
      -- Then Bytes 4-7 become padding.
      place :: forall t r. Int -> KernelArgR t r -> IO (Int, Int)
      place cursor argument = do
        -- width is the size of the field
        -- align tells the program which byte offsets are valid
        let (align, width) = kernelArgLayout argument
            offset = alignArgumentOffset cursor align
        -- Validates whether the field fits within the metadata's defined size
        when (offset < 0 || width > bytes || offset > bytes - width) $
          internalError "Metal argument write exceeds allocation"
        pure (offset, offset + width)
