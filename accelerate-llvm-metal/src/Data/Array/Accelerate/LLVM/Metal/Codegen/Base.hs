{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.LLVM.Metal.CodeGen.Base
  ( codeGenKernel
  -- , generateArgumentMetadata
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
import Data.Array.Accelerate.Error (internalError)
import Control.Monad (unless)

metadataNode :: [Maybe Metadata] -> CodeGen Metal Metadata
metadataNode fields = do
  nodeId <- addMetadata (const fields)
  pure $ MetadataNodeOperand $ MetadataNodeReference nodeId

int32Metadata :: Int32 -> Metadata
int32Metadata = MetadataConstantOperand . ScalarConstant scalarTypeInt32

codeGenKernel
  :: forall f a. Result f ~ ()
  => String
  -> (forall k. Function k () -> Function k f)
  -> CodeGen Metal [Metadata]
  -> CodeGen Metal a
  -> LLVM Metal (a, Module f)
codeGenKernel name args mkArgMetadata body =
  codeGenFunction Nothing name VoidType args $ do
    returnDescription <- metadataNode []
    argumentDescription <- mkArgMetadata
    argumentsDescription <- metadataNode $ map Just argumentDescription

    let functionReference = MetadataPretty $ LP.ValMdValue $ LP.Typed  (LP.decFunType (downcast declare)) (LP.ValSymbol (fromString name))

    addNamedMetadata "air.kernel" [Just functionReference, Just returnDescription, Just argumentsDescription]
    addNamedMetadata "air.version" (map (Just . int32Metadata) [2, 8, 0])

    addNamedMetadata "air.language_version" [Just (MetadataStringOperand "Metal"), Just (int32Metadata 4), Just (int32Metadata 0), Just (int32Metadata 0)]

    body
  where
    declare :: GlobalFunction f
    declare = args $ Body VoidType Nothing (fromString name)

-- generateArgumentMetadata :: Int -> Int -> CodeGen Metal [Metadata]
-- generateArgumentMetadata extentField outputField = do
--   unless ((extentField, outputField) == (0, 1) || (extentField, outputField) == (1, 0))
--     $ internalError "Expected two distinct Generate argument fields"
--
--     -- , "!3 = !{i32 0, !\"air.indirect_buffer\", !\"air.buffer_size\", i32 16, !\"air.location_index\", i32 0, i32 1, !\"air.read\", !\"air.address_space\", i32 2, !\"air.struct_type_info\", !5, !\"air.arg_type_size\", i32 16, !\"air.arg_type_align_size\", i32 8, !\"air.arg_type_name\", !\"Args\", !\"air.arg_name\", !\"args\"}"
--     -- , "!4 = !{i32 1, !\"air.thread_position_in_grid\", !\"air.arg_type_name\", !\"uint\"}"
--     -- , "!5 = !{" ++ intercalate ", " (map member [0, 1]) ++ "}"
--     -- , "!6 = !{i32 2, i32 8, i32 0}"
--     -- , "!7 = !{!\"Metal\", i32 4, i32 0, i32 0}"
--     -- , "!8 = !{i32 " ++ show outputField
--     --     ++ ", !\"air.buffer\", !\"air.location_index\", i32 "
--     --     ++ show outputField
--     --     ++ ", i32 1, !\"air.read_write\", !\"air.address_space\", i32 1, !\"air.arg_type_size\", i32 4, !\"air.arg_type_align_size\", i32 4, !\"air.arg_type_name\", !\"int\", !\"air.arg_name\", !\"output\"}"
--     -- , "!9 = !{i32 " ++ show extentField
--     --     ++ ", !\"air.indirect_constant\", !\"air.location_index\", i32 "
--     --     ++ show extentField
--     --     ++ ", i32 1, !\"air.arg_type_name\", !\"long\", !\"air.arg_name\", !\"extent\"}"
--
--   outputDesc <- undefined
