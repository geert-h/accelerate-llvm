module Data.Array.Accelerate.LLVM.Metal.Link.Object
  ( KernelObject(..)
  , withKernelObject
  , releaseKernelObject
  ) where

import Foreign.ForeignPtr
  ( ForeignPtr
  , withForeignPtr
  , finalizeForeignPtr
  )
import Foreign.Ptr (Ptr)

import Data.Array.Accelerate.LLVM.Metal.Context (Context)
import Data.Array.Accelerate.LLVM.Metal.FFI (RawPipeline)

data KernelObject = KernelObject
  { kernelObjName     :: String
  , kernelObjContext  :: Context
  , kernelObjPipeline :: ForeignPtr RawPipeline
  }

withKernelObject :: KernelObject -> (Ptr RawPipeline -> IO a) -> IO a
withKernelObject kernel = withForeignPtr (kernelObjPipeline kernel)

releaseKernelObject :: KernelObject -> IO ()
releaseKernelObject = finalizeForeignPtr . kernelObjPipeline
