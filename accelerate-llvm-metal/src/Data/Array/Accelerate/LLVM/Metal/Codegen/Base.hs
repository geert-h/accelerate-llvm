{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.LLVM.Metal.CodeGen.Base
  ( codeGenKernel
  -- , Header
  , KernelType
  ) where

import Data.Array.Accelerate.LLVM.Metal.Target (Metal)
import Data.Array.Accelerate.LLVM.CodeGen.Monad


import Data.Array.Accelerate.LLVM.State

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.Global
import LLVM.AST.Type.Metadata
import LLVM.AST.Type.Module
import LLVM.AST.Type.Representation

import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty     as LP
import Data.String
import Data.Maybe
import Prelude                                                      as P
import Data.Array.Accelerate.LLVM.CodeGen.Environment

-- -- The struct passed as argument to a call contains:
-- --  * work_function: ptr
-- --  * continuation: ptr, u32 (program, location)
-- --  * active_threads: u32,
-- --  * work_index: u64,
-- --  * work_per_thread: ptr (to array of u64),
-- --  * tracy_srcloc: ptr,
-- --  * In the future, perhaps also store a work_size: u32
-- -- We store the work function as a pointer to a struct, as that makes it easy
-- -- to separate pointers to a kernel from pointers to buffers, when compiling
-- -- a schedule.
-- type Header = ((((((Ptr (Struct Int8), Ptr Int8), Word32), Word32), Word64), Ptr Word64), Ptr TracySrcloc)
--
-- type KernelType env
--   -- Ptr to the kernel struct
--   = Ptr (Struct ((Header, Struct (MarshalEnv env)), SizedArray Word))
--   -- Ptr to the locks array (for any permutes)
--   -> Ptr Word8
--   -- thread_index, or a magic value for single-threaded initialization or finalization
--   -> Word32
--   -- max_thread_count
--   -> Word32
--   -- Only in initialization, this function returns whether the kernel should run sequentially or in parallel
--   -> Word8

type KernelType env = Ptr (Struct (MarshalEnv env)) -> Word32 -> ()

codeGenKernel
  :: forall f a. Result f ~ ()
  => String
  -> (forall k. Function k () -> Function k f)
  -> CodeGen Metal a
  -> CodeGen Metal [MetadataNodeID]
  -> LLVM Metal (a, Module f)
codeGenKernel name args body metadata =
  codeGenFunction Nothing name VoidType args $ do
    result <- body -- calls declareAliasScopes, so it needs to be the first to add metadata
    returnDescription <- addMetadata (const [])

    argDescs <- metadata
    argumentsDescription <- addMetadata $ const $ map id2ref argDescs

    let functionReference = MetadataPretty $ LP.ValMdValue $ LP.Typed  (LP.decFunType (downcast declare)) (LP.ValSymbol (fromString name))

    addNamedMetadata "air.kernel" [Just functionReference, id2ref returnDescription, id2ref argumentsDescription]
    addNamedMetadata "air.version" (map (Just . int2metadata) [2, 8, 0])

    addNamedMetadata "air.language_version" [Just (MetadataStringOperand "Metal"), Just (int2metadata 4), Just (int2metadata 0), Just (int2metadata 0)]
    
    -- TODO: these values should probably be retrieved by asking the OS, for now it is hardcoded
    -- it should have the shape: !llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8}
    -- !0 = !{i32 2, !"SDK Version", [2 x i32] [i32 26, i32 5]}
    let i32 = LP.PrimType (LP.Integer 32)
    addNamedMetadata "llvm.module.flags"
      [ Just (int2metadata 2)
      , Just (MetadataStringOperand "SDK Version")
      , Just $ MetadataPretty $ LP.ValMdValue $ LP.Typed (LP.Array 2 i32) (LP.ValArray i32 [LP.ValInteger 26, LP.ValInteger 5])]

    -- !1 = !{i32 1, !"wchar_size", i32 4}
    addNamedMetadata "llvm.module.flags" [Just (int2metadata 1), Just (MetadataStringOperand "wchar_size"), Just (int2metadata 4)]

    -- !2 = !{i32 7, !"frame-pointer", i32 2}
    addNamedMetadata "llvm.module.flags" [Just (int2metadata 7), Just (MetadataStringOperand "frame-pointer"), Just (int2metadata 2)]

    -- !3 = !{i32 7, !"air.max_device_buffers", i32 31}
    addNamedMetadata "llvm.module.flags" [Just (int2metadata 7), Just (MetadataStringOperand "air.max_device_buffers"), Just (int2metadata 31)]

    -- !4 = !{i32 7, !"air.max_constant_buffers", i32 31}
    addNamedMetadata "llvm.module.flags" [Just (int2metadata 7), Just (MetadataStringOperand "air.max_constant_buffers"), Just (int2metadata 31)]

    -- !5 = !{i32 7, !"air.max_threadgroup_buffers", i32 31}
    addNamedMetadata "llvm.module.flags" [Just (int2metadata 7), Just (MetadataStringOperand "air.max_threadgroup_buffers"), Just (int2metadata 31)]

    -- !6 = !{i32 7, !"air.max_textures", i32 128}
    addNamedMetadata "llvm.module.flags" [Just (int2metadata 7), Just (MetadataStringOperand "air.max_textures"), Just (int2metadata 128)]

    -- !7 = !{i32 7, !"air.max_read_write_textures", i32 8}
    addNamedMetadata "llvm.module.flags" [Just (int2metadata 7), Just (MetadataStringOperand "air.max_read_write_textures"), Just (int2metadata 8)]
    -- !8 = !{i32 7, !"air.max_samplers", i32 16}
    addNamedMetadata "llvm.module.flags" [Just (int2metadata 7), Just (MetadataStringOperand "air.max_samplers"), Just (int2metadata 18)]


    pure result
  where
    declare :: GlobalFunction f
    declare = args $ Body VoidType Nothing (fromString name)

    id2ref = Just . MetadataNodeOperand . MetadataNodeReference 

    int2metadata = MetadataConstantOperand . ScalarConstant scalarTypeInt32

