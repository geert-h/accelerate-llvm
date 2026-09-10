{-# LANGUAGE OverloadedStrings #-}

module Data.Array.Accelerate.LLVM.Metal.Array.Prim
  ( mallocBuffer
  , withNewBuffer
  , copyToHost
  ) where

import Control.Exception (bracket, mask_, onException)
import Control.Monad (when)
import Foreign.C.String (peekCString)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)

import Data.Array.Accelerate.LLVM.Metal.Array.Data
import Data.Array.Accelerate.LLVM.Metal.Context
import Data.Array.Accelerate.LLVM.Metal.FFI
import Formatting (string)
import Data.Array.Accelerate.Error (internalError)

mallocBuffer :: Context -> Int -> IO MetalBuffer
mallocBuffer context bytes
  | bytes < 0 = internalError "Negative buffer size"
  | bytes == 0 = pure (MetalBuffer context 0 Nothing)
  | otherwise = mask_ $
    withContext context $ \rawContext ->
      allocaBytes 1024 $ \errorBuffer -> do
        pointer <- bufferCreate rawContext (fromIntegral bytes) errorBuffer 1024

        if pointer == nullPtr
            then do
              internalError string <$> peekCString errorBuffer
            else do
              handle <- newForeignPtr bufferFinalizer pointer
                `onException` bufferDestroy pointer
              pure (MetalBuffer context bytes (Just handle))

withNewBuffer :: Context -> Int -> (MetalBuffer -> IO a) -> IO a
withNewBuffer context bytes = bracket (mallocBuffer context bytes) releaseBuffer

-- GPU writes must have completed before calling this
copyToHost :: MetalBuffer -> Int -> Ptr a -> IO ()
copyToHost buffer bytes destination
  | bytes < 0 || bytes > bufferBytes buffer = internalError "Buffer read exceeds allocation"
  | bytes == 0 = pure ()
  | otherwise =
    withBuffer buffer $ \pointer ->
      allocaBytes 1024 $ \errorBuffer -> do
        status <- bufferRead pointer (castPtr destination) (fromIntegral bytes) errorBuffer 1024
        when (status /= 0) $ do
          internalError string <$> peekCString errorBuffer

