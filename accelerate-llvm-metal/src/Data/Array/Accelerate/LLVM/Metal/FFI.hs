{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Array.Accelerate.LLVM.Metal.FFI
  ( RawContext
  , contextCreate
  , contextDestroy
  , contextFinalizer
  , RawPipeline
  , pipelineLoad
  , pipelineDestroy
  , pipelineFinalizer
  , generateI32
  , RawBuffer
  , bufferCreate
  , bufferDestroy
  , bufferFinalizer
  , bufferRead
  , bufferContents
  , bufferGPUAddress
  , runKernel
  ) where

import Foreign.C.String   (CString)
import Foreign.C.Types    (CInt(..), CSize(..))
import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Ptr        (Ptr)

import Data.Word (Word32, Word64)

data RawContext

foreign import ccall safe "acc_metal_context_construct"
  contextCreate :: CString -> CSize -> IO (Ptr RawContext)

foreign import ccall unsafe "acc_metal_context_deconstruct"
  contextDestroy :: Ptr RawContext -> IO ()

foreign import ccall unsafe "&acc_metal_context_deconstruct"
  contextFinalizer :: FinalizerPtr RawContext

data RawPipeline

foreign import ccall safe "acc_metal_pipeline_load"
  pipelineLoad
    :: Ptr RawContext
    -> CString
    -> CString
    -> CString
    -> CSize
    -> IO (Ptr RawPipeline)

foreign import ccall unsafe "acc_metal_pipeline_deconstruct"
  pipelineDestroy :: Ptr RawPipeline -> IO ()

foreign import ccall unsafe "&acc_metal_pipeline_deconstruct"
  pipelineFinalizer :: FinalizerPtr RawPipeline

data RawBuffer

foreign import ccall safe "acc_metal_buffer_construct"
  bufferCreate
    :: Ptr RawContext
    -> CSize
    -> CString
    -> CSize
    -> IO (Ptr RawBuffer)

foreign import ccall unsafe "acc_metal_buffer_deconstruct"
  bufferDestroy :: Ptr RawBuffer -> IO ()

foreign import ccall unsafe "&acc_metal_buffer_deconstruct"
  bufferFinalizer :: FinalizerPtr RawBuffer

foreign import ccall safe "acc_metal_buffer_read"
  bufferRead
    :: Ptr RawBuffer
    -> Ptr ()
    -> CSize
    -> CString
    -> CSize
    -> IO CInt

foreign import ccall safe "acc_metal_generate_i32"
  generateI32
    :: Ptr RawContext
    -> Ptr RawPipeline
    -> Word32
    -> Ptr RawBuffer
    -> CString
    -> CSize
    -> IO CInt

foreign import ccall safe "acc_metal_buffer_contents"
  bufferContents
    :: Ptr RawBuffer
    -> CString
    -> CSize
    -> IO (Ptr ())

foreign import ccall safe "acc_metal_buffer_gpu_address"
  bufferGPUAddress
  :: Ptr RawBuffer
  -> Ptr Word64
  -> CString
  -> CSize
  -> IO CInt

foreign import ccall safe "acc_metal_run_kernel"
  runKernel
    :: Ptr RawContext
    -> Ptr RawPipeline
    -> Ptr RawBuffer
    -> Word32
    -> Ptr (Ptr RawBuffer)
    -> CSize
    -> CString
    -> CSize
    -> IO CInt

