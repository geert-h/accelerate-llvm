{-# LANGUAGE OverloadedStrings #-}

module Data.Array.Accelerate.LLVM.Metal.Execute.Generate
  ( launchGenerateI32
  ) where

import Control.Monad (when)
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (sizeOf)

import Data.Array.Accelerate.LLVM.Metal.Array.Data
import Data.Array.Accelerate.LLVM.Metal.Context (withContext)
import Data.Array.Accelerate.LLVM.Metal.FFI (generateI32)
import Data.Array.Accelerate.LLVM.Metal.Link.Object
import Data.Array.Accelerate.Error (internalError)
import Formatting (string)

-- This function is synchronous, it only returns after the GPU finishes
-- Requires the fixture ABI:
-- buffer 0 = Int32 output
-- buffer 1 = Word32 element count
launchGenerateI32 :: KernelObject -> MetalBuffer -> Int -> IO ()
launchGenerateI32 kernel output n
  | n < 0 = internalError "Negative element count"
  | toInteger n > toInteger (maxBound:: Word32) = internalError "Element count exceeds Word32"
  | n > bufferBytes output `div` sizeOf (undefined :: Int32) = internalError "Host output size overflow"
  | n == 0 = pure ()
  | otherwise =
    withContext (kernelObjContext kernel) $ \context ->
      withKernelObject kernel $ \pipeline ->
        withBuffer output $ \buffer ->
          allocaBytes 1024 $ \errorBuffer -> do
            status <- generateI32 context pipeline
              (fromIntegral n) buffer errorBuffer 1024
            when (status /= 0) $ do
              message <- peekCString errorBuffer
              internalError string message
          
