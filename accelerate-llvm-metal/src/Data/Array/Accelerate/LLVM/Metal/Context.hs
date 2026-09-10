module Data.Array.Accelerate.LLVM.Metal.Context
  ( Context
  , new
  , withContext
  , withNewContext
  ) where

import Control.Exception (bracket, mask_, onException)
import Foreign.C.String (peekCString)
import Foreign.ForeignPtr
  ( ForeignPtr
  , finalizeForeignPtr
  , newForeignPtr
  , withForeignPtr
  )
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)

import Data.Array.Accelerate.LLVM.Metal.FFI
import Data.Array.Accelerate.Error (internalError)
import Formatting (string)

newtype Context = Context (ForeignPtr RawContext)

new :: IO Context
new = mask_ $
  allocaBytes 256 $ \errorBuffer -> do
    pointer <- contextCreate errorBuffer 256
    if pointer == nullPtr
     then do
       internalError string <$> peekCString errorBuffer
     else do
       -- make new context, if fails the destroy
       handle <- newForeignPtr contextFinalizer pointer `onException` contextDestroy pointer
       pure (Context handle)

withContext :: Context -> (Ptr RawContext -> IO a) -> IO a
withContext (Context handle) = withForeignPtr handle

withNewContext :: (Context -> IO a) -> IO a
withNewContext = bracket new release
  where
    release (Context handle) = finalizeForeignPtr handle

