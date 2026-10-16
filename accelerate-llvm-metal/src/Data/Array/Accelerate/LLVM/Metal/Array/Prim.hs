{-# LANGUAGE OverloadedStrings #-}

module Data.Array.Accelerate.LLVM.Metal.Array.Prim
  ( mallocBuffer
  , withNewBuffer
  , copyToHost
  , readFromDevice
  , copyToDevice
  , withBufferContents
  , withBufferGPUAddress
  ) where

import Control.Exception (bracket, mask_, onException)
import Control.Monad (when)
import Foreign.C.String (peekCString)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peek)

import Data.Array.Accelerate.LLVM.Metal.Array.Data
import Data.Array.Accelerate.LLVM.Metal.Context
import Data.Array.Accelerate.LLVM.Metal.FFI
import Formatting (string)
import Data.Array.Accelerate.Error (internalError)
import Foreign.Marshal.Utils (copyBytes)
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Representation.Elt (scalarTypeSize)

mallocBuffer :: Context -> Int -> IO MetalBuffer
mallocBuffer context bytes
  | bytes < 0 = internalError "Negative buffer size"
  | bytes == 0 = pure (MetalBuffer context 0 Nothing)
  | otherwise = mask_ $
    withContext context $ \rawContext ->
      allocaBytes 1024 $ \errorBuffer -> do
        pointer <- bufferCreate rawContext (fromIntegral bytes) errorBuffer 1024
        if pointer == nullPtr
            then do peekCString errorBuffer >>= internalError string
            else do
              handle <- newForeignPtr bufferFinalizer pointer
                `onException` bufferDestroy pointer
              pure (MetalBuffer context bytes (Just handle))

withNewBuffer :: Context -> Int -> (MetalBuffer -> IO a) -> IO a
withNewBuffer context bytes = bracket (mallocBuffer context bytes) releaseBuffer

copyToDevice :: MetalBuffer -> Int -> Ptr a -> IO ()
copyToDevice buffer bytes source
  | bytes < 0 || bytes > bufferBytes buffer = internalError "Buffer read exceeds allocation"
  | bytes == 0 = pure ()
  | otherwise =
    withBufferContents buffer $ \destination ->
      copyBytes destination (castPtr source) bytes

readFromDevice :: ScalarType e -> MetalBuffer -> Int -> IO e
readFromDevice tp buffer idx 
  | ScalarDict <- scalarDict tp =
    alloca $ \dest -> do
      copyFromBufferIndexToDevice buffer (scalarTypeSize tp) idx dest
      peek dest

copyFromBufferIndexToDevice :: MetalBuffer -> Int -> Int -> Ptr a -> IO ()
copyFromBufferIndexToDevice buffer bytes idx dest
  | idx < 0 || idx > bufferBytes buffer = internalError "Index outside of bounds metal buffer on device"
  | otherwise =
    withBuffer buffer $ \ptr ->
      allocaBytes 1024 $ \errorBuffer -> do
        status <- bufferReadIndex ptr (castPtr dest) (fromIntegral bytes) (fromIntegral idx) errorBuffer 1024
        when (status /= 0) $ do peekCString errorBuffer >>= internalError string

-- GPU writes must have completed before calling this
copyToHost :: MetalBuffer -> Int -> Ptr a -> IO ()
copyToHost buffer bytes destination
  | bytes < 0 || bytes > bufferBytes buffer = internalError "Buffer read exceeds allocation"
  | bytes == 0 = pure ()
  | otherwise =
    withBuffer buffer $ \pointer ->
      allocaBytes 1024 $ \errorBuffer -> do
        status <- bufferRead pointer (castPtr destination) (fromIntegral bytes) errorBuffer 1024
        when (status /= 0) $ do peekCString errorBuffer >>= internalError string 

withBufferContents :: MetalBuffer -> (Ptr () -> IO a) -> IO a
withBufferContents buffer action =
  withBuffer buffer $ \raw -> allocaBytes 1024 $ \errorBuffer -> do
    contents <- bufferContents raw errorBuffer 1024
    if contents == nullPtr
      then peekCString errorBuffer >>= internalError string
      else action contents

withBufferGPUAddress :: MetalBuffer -> (Word64 -> IO a) -> IO a
withBufferGPUAddress buffer action =
  withBuffer buffer $ \raw ->
    if raw == nullPtr then action 0 else
      alloca $ \address -> allocaBytes 1024 $ \errorBuffer -> do
        status <- bufferGPUAddress raw address errorBuffer 1024
        if status /= 0
          then peekCString errorBuffer >>= internalError string
          else peek address >>= action
      
