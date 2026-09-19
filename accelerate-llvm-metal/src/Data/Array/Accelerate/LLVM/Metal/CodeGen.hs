{-# LANGUAGE GADTs #-}

module Data.Array.Accelerate.LLVM.Metal.CodeGen
  ( MetalCode(..)
  , codegen
  ) where

import Data.Int (Int32)
import Data.Type.Equality ((:~:)(Refl))

import Data.Array.Accelerate.Array.Buffer (Buffer)
import Data.Array.Accelerate.Analysis.Match (matchScalarType)
import Data.Array.Accelerate.AST.Environment (Env (..))
import Data.Array.Accelerate.AST.Idx
    ( Idx, matchIdx, matchIdx, idxToInt )
import Data.Array.Accelerate.AST.LeftHandSide (LeftHandSide(..))
import Data.Array.Accelerate.AST.Partitioned
import Data.Array.Accelerate.Representation.Array (ArrayR(..))
import Data.Array.Accelerate.Representation.Shape (ShapeR(..), DIM1)
import Data.Array.Accelerate.Representation.Type (TupR(..))
import Data.Array.Accelerate.Type (scalarTypeInt32, scalarTypeInt)

import Data.Array.Accelerate.LLVM.State (LLVM)
import Data.Array.Accelerate.LLVM.Metal.Operation (MetalOp(..))
import Data.Array.Accelerate.LLVM.Metal.Target (Metal)
import Data.Array.Accelerate.Error (internalError)
import Formatting (string)

import Data.List (intercalate)

data MetalCode env = MetalCode
  { metalCodeSource   :: !String
  , metalCodeName     :: !String
  , metalCodeElements :: ![Idx env Int]
  , metalCodeOutput   :: !(Idx env (Buffer Int32))
  }

codegen :: String -> Env AccessGroundR env -> Clustered MetalOp args -> Args env args -> LLVM Metal (MetalCode env)
codegen name parameterTypes cluster args = codegenIndependent name parameterTypes (toFlatClustered cluster args)

-- Currently this function only accepts one very restrictive generate case
codegenIndependent :: String -> Env AccessGroundR env -> FlatCluster MetalOp env -> LLVM Metal (MetalCode env)
codegenIndependent name parameterTypes flat =
  case flat of
    FlatCluster
      (ShapeRsnoc ShapeRz)                                           -- Checks whether the shape is one-dimensional
      _indexLHS                                                      -- Ignore the loop for now
      (TupRpair TupRunit (TupRsingle (Var _ extent)))                -- defines the length represented as ((), length). extent identifies the kernel param that will contain the length
      (TupRpair TupRunit (TupRsingle LoopAny))                       -- Says that the order of execution doesn't matter (which is the case for generate)
      TupRunit                                                       -- The cluster has no local buffer types
      (LeftHandSideWildcard TupRunit)                                -- no local variables are added to the param env
      (FlatOpsOp                                                     -- This is the generate operation
        (FlatOp MetalGenerate
          (ArgFun function                                          -- Generate has a function and an output array
            :>: ArgArray Out                                         -- One output array
              (ArrayR (ShapeRsnoc ShapeRz) (TupRsingle elementType)) -- The output is one-dimensional, just like the input and each element is a single scalar
              (TupRpair TupRunit
                (TupRsingle (Var _ outputExtent)))                   -- param index containing the output length
              buffers                                                -- identifies the output's underlying storage
            :>: ArgsNil)
          (IdxArgNone :>: IdxArgIdx 1 _indices :>: ArgsNil))         -- the function argument has not array-index annotation
        FlatOpsNil)
      | Just Refl <- matchScalarType elementType scalarTypeInt32     -- Checks that the element is type Int32
      , TupRsingle (Var _ output) <- buffers                         -- extracts the single output-buffer var
      , Just Refl <- matchIdx extent outputExtent                    -- checks whether the iteration length and output length reference the same var
      -> do
        fields <- generateArgumentTypes parameterTypes
        
        -- TODO: some correctness checking here maybe
        value <- constantGenerator function

        let extentField = length fields - 1 - idxToInt extent
            outputField = length fields - 1 - idxToInt output

        pure MetalCode
          { metalCodeSource = renderGenerate name fields extentField outputField value
          , metalCodeName = name
          , metalCodeElements = [extent]
          , metalCodeOutput = output
          }
    _ -> stop "unsupported cluster: expected one independent, one-dimensional Int32 Generate with no local buffers, or index bindings, and matching iteration/output extents"

generateArgumentTypes :: Env AccessGroundR env -> LLVM Metal [String]
generateArgumentTypes Empty = pure []
generateArgumentTypes (Push env argument) = do
  fields <- generateArgumentTypes env -- recursively call argument generation
  field <- case argument of
    AccessGroundRscalar tp
      | Just Refl <- matchScalarType tp scalarTypeInt -> pure "i64" -- for now only accept 64 bit integer 
    AccessGroundRbuffer _ tp
      | Just Refl <- matchScalarType tp scalarTypeInt32 -> pure "ptr addrspace(1)" -- for now only accept 32 bit integer buffer values
    _ -> stop "unsupported Generate argument type"
  pure (fields ++ [field])

-- Currently only accepts a list of integers
constantGenerator :: Fun env (DIM1 -> Int32) -> LLVM Metal Int32
constantGenerator (Lam _ (Body (Const _ value))) = pure value
constantGenerator _ = stop "only an Int32 constant generator is implemented"

-- This function spits out very minimal .ll code to work with the generate operation
renderGenerate :: String -> [String] -> Int -> Int -> Int32 -> String
renderGenerate name fields extentField outputField value = unlines
    [ "target datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:256-v256:256:256-v512:512:512-v1024:1024:1024-n8:16:32\""
    , "target triple = \"air64_v28-apple-macosx26.0.0\""
    , ""
    , "%Args = type { " ++ intercalate ", " fields ++ " }" -- add all the fields to the arg buffer
    , ""
    , "define void @" ++ name ++ "(ptr addrspace(2) %args, i32 %gid) {"
    , "entry:"
    , "  %extent.slot = getelementptr %Args, ptr addrspace(2) %args, i32 0, i32 " ++ show extentField
    , "  %n = load i64, ptr addrspace(2) %extent.slot, align 8"
    , "  %index = zext i32 %gid to i64"
    , "  %inside = icmp slt i64 %index, %n"
    , "  br i1 %inside, label %write, label %done"
    , ""
    , "write:"
    , "  %output.slot = getelementptr %Args, ptr addrspace(2) %args, i32 0, i32 " ++ show outputField
    , "  %output = load ptr addrspace(1), ptr addrspace(2) %output.slot, align 8"
    , "  %value = call i32 @generate_element(i64 %index)"
    , "  %destination = getelementptr i32, ptr addrspace(1) %output, i64 %index"
    , "  store i32 %value, ptr addrspace(1) %destination, align 4"
    , "  br label %done"
    , ""
    , "done:"
    , "  ret void"
    , "}"
    , ""
    , "define internal i32 @generate_element(i64 %index) {"
    , "entry:"
    , "  ret i32 " ++ show value
    , "}"
    , ""
    , "!air.kernel = !{!0}"
    , "!air.version = !{!6}"
    , "!air.language_version = !{!7}"
    , "!0 = !{ptr @" ++ name ++ ", !1, !2}"
    , "!1 = !{}"
    , "!2 = !{!3, !4}"
    , "!3 = !{i32 0, !\"air.indirect_buffer\", !\"air.buffer_size\", i32 16, !\"air.location_index\", i32 0, i32 1, !\"air.read\", !\"air.address_space\", i32 2, !\"air.struct_type_info\", !5, !\"air.arg_type_size\", i32 16, !\"air.arg_type_align_size\", i32 8, !\"air.arg_type_name\", !\"Args\", !\"air.arg_name\", !\"args\"}"
    , "!4 = !{i32 1, !\"air.thread_position_in_grid\", !\"air.arg_type_name\", !\"uint\"}"
    , "!5 = !{" ++ intercalate ", " (map member [0, 1]) ++ "}"
    , "!6 = !{i32 2, i32 8, i32 0}"
    , "!7 = !{!\"Metal\", i32 4, i32 0, i32 0}"
    , "!8 = !{i32 " ++ show outputField
        ++ ", !\"air.buffer\", !\"air.location_index\", i32 "
        ++ show outputField
        ++ ", i32 1, !\"air.read_write\", !\"air.address_space\", i32 1, !\"air.arg_type_size\", i32 4, !\"air.arg_type_align_size\", i32 4, !\"air.arg_type_name\", !\"int\", !\"air.arg_name\", !\"output\"}"
    , "!9 = !{i32 " ++ show extentField
        ++ ", !\"air.indirect_constant\", !\"air.location_index\", i32 "
        ++ show extentField
        ++ ", i32 1, !\"air.arg_type_name\", !\"long\", !\"air.arg_name\", !\"extent\"}"
    ]
  where
    -- builds metadata describing a field in the argument buffer
    -- For example, it generates:
    -- ; Field 0: output-buffer address
    -- i32 0, i32 8, i32 0, !"int", !"output" !"air.indirect_argument", !9
    --
    -- ; Field 1: extent for the output buffer
    -- i32 8, i32 8, i32 0, !"long", !"extent" !"air.indirect_argument", !8
    member i = -- i is either 0 or 1
      "i32 " ++ show (8 * i) ++ ", i32 8, i32 0, "
        ++ if i == outputField
             then "!\"int\", !\"output\", !\"air.indirect_argument\", !8"
             else "!\"long\", !\"extent\", !\"air.indirect_argument\", !9"

stop :: String -> LLVM Metal a
stop msg = internalError string $ "accelerate-llvm-metal: " ++ msg

