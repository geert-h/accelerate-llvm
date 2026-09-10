module Data.Array.Accelerate.LLVM.Metal.Array.Data
  ( MetalBuffer(..)
  , withBuffer
  , releaseBuffer
  ) where

import Foreign.ForeignPtr
  ( ForeignPtr
  , withForeignPtr
  , finalizeForeignPtr
  )
import Foreign.Ptr (Ptr, nullPtr)

import Data.Array.Accelerate.LLVM.Metal.Context (Context, withContext)
import Data.Array.Accelerate.LLVM.Metal.FFI (RawBuffer)

data MetalBuffer = MetalBuffer
  { bufferContext :: !Context
  , bufferBytes   :: !Int
  , bufferHandle  :: !(Maybe (ForeignPtr RawBuffer))
  }

-- the pointer must not escape the callback
withBuffer :: MetalBuffer -> (Ptr RawBuffer -> IO a) -> IO a
withBuffer buffer action =
  withContext (bufferContext buffer) $ \_ ->
    case bufferHandle buffer of
      Nothing     -> action nullPtr
      Just handle -> withForeignPtr handle action

releaseBuffer :: MetalBuffer -> IO ()
releaseBuffer = maybe (pure ()) finalizeForeignPtr . bufferHandle


