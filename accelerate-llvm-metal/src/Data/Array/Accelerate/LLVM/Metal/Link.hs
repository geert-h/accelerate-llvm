module Data.Array.Accelerate.LLVM.Metal.Link
  ( KernelObject
  , loadKernel
  , withLoadedKernel
  , link
  ) where

import Control.Exception (bracket, mask_, onException)
import Foreign.C.String (withCString, peekCString)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Data.Array.Accelerate.LLVM.State (LLVM)

import Data.Array.Accelerate.LLVM.Metal.Context
import Data.Array.Accelerate.LLVM.Metal.FFI
import Data.Array.Accelerate.LLVM.Metal.Link.Object
import Data.Array.Accelerate.LLVM.Metal.Target (Metal, metalContext)
import Data.Array.Accelerate.Error (internalError)
import Formatting (string)
import Data.Array.Accelerate.LLVM.Metal.Compile (ObjectR(..))
import qualified Data.ByteString.Short.Char8 as SBS

loadKernel :: Context -> FilePath -> String -> IO KernelObject
loadKernel context path name = mask_ $
  withContext context $ \rawContext ->
    withCString path $ \rawPath ->
      withCString name $ \rawName ->
        allocaBytes 1024 $ \errorBuffer -> do
          ptr <- pipelineLoad rawContext rawPath rawName errorBuffer 1024

          if ptr == nullPtr
            then do
              internalError string <$> peekCString errorBuffer
            else do
              handle <- newForeignPtr pipelineFinalizer ptr `onException` pipelineDestroy ptr
              pure (KernelObject name context handle)

-- The kernel must not escape this callback.
withLoadedKernel
  :: Context
  -> FilePath
  -> String
  -> (KernelObject -> IO a)
  -> IO a
withLoadedKernel context path name =
  bracket (loadKernel context path name) releaseKernelObject

link :: ObjectR f -> LLVM Metal KernelObject
link object = do
  context <- asks metalContext
  liftIO $ loadKernel context (objPath object) (SBS.unpack (objSym object))
