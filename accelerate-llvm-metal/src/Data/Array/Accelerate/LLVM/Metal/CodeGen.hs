{-# LANGUAGE GADTs #-}

module Data.Array.Accelerate.LLVM.Metal.CodeGen
  ( MetalCode(..)
  , codegen
  ) where

import Data.Int (Int32)
import Data.Type.Equality ((:~:)(Refl))

import Data.Array.Accelerate.Array.Buffer (Buffer)
import Data.Array.Accelerate.Analysis.Match (matchScalarType)
import Data.Array.Accelerate.AST.Environment (Env)
import Data.Array.Accelerate.AST.Idx
    ( Idx, matchIdx, matchIdx )
import Data.Array.Accelerate.AST.LeftHandSide (LeftHandSide(..))
import Data.Array.Accelerate.AST.Partitioned
import Data.Array.Accelerate.Representation.Array (ArrayR(..))
import Data.Array.Accelerate.Representation.Shape (ShapeR(..), DIM1)
import Data.Array.Accelerate.Representation.Type (TupR(..))
import Data.Array.Accelerate.Type (scalarTypeInt32)

import Data.Array.Accelerate.LLVM.State (LLVM)
import Data.Array.Accelerate.LLVM.Metal.Operation (MetalOp(..))
import Data.Array.Accelerate.LLVM.Metal.Target (Metal)
import Data.Array.Accelerate.Error (internalError)
import Formatting (string)

data MetalCode env = MetalCode
  { metalCodeSource   :: !String
  , metalCodeName     :: !String
  , metalCodeElements :: ![Idx env Int]
  , metalCodeOutput   :: !(Idx env (Buffer Int32))
  }

codegen :: String -> Env AccessGroundR env -> Clustered MetalOp args -> Args env args -> LLVM Metal (MetalCode env)
codegen name _parameterTypes cluster args = codegenIndependent name (toFlatClustered cluster args)

-- Currently this function only accepts one very restrictive generate case
codegenIndependent :: String -> FlatCluster MetalOp env -> LLVM Metal (MetalCode env)
codegenIndependent name flat =
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
        value <- constantGenerator function
        pure MetalCode
          { metalCodeSource = renderGenerate name value
          , metalCodeName = name
          , metalCodeElements = [extent]
          , metalCodeOutput = output
          }
    _ -> stop "unsupported cluster: expected one independent, one-dimensional Int32 Generate with no local buffers, or index bindings, and matching iteration/output extents"

-- Currently only accepts a list of integers
constantGenerator :: Fun env (DIM1 -> Int32) -> LLVM Metal Int32
constantGenerator (Lam _ (Body (Const _ value))) = pure value
constantGenerator _ = stop "only an Int32 constant generator is implemented"

-- This function spits out very minimal .ll code to work with the generate operation
renderGenerate :: String -> Int32 -> String
renderGenerate name value = unlines
    [ "target datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v24:32:32-v32:32:32-v48:64:64-v64:64:64-v96:128:128-v128:128:128-v192:256:256-v256:256:256-v512:512:512-v1024:1024:1024-n8:16:32\""
    , "target triple = \"air64_v28-apple-macosx26.0.0\""
    , ""
    , "define void @" ++ name
        ++ "(i32 addrspace(1)* %output, i32 addrspace(2)* %count, i32 %gid) {"
    , "entry:"
    , "  %n = load i32, i32 addrspace(2)* %count, align 4"
    , "  %inside = icmp ult i32 %gid, %n"
    , "  br i1 %inside, label %write, label %done"
    , ""
    , "write:"
    , "  %index = zext i32 %gid to i64"
    , "  %value = call i32 @generate_element(i64 %index)"
    , "  %destination = getelementptr i32, i32 addrspace(1)* %output, i64 %index"
    , "  store i32 %value, i32 addrspace(1)* %destination, align 4"
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
    , "!0 = !{void (i32 addrspace(1)*, i32 addrspace(2)*, i32)* @" ++ name ++ ", !1, !2}"
    , "!1 = !{}"
    , "!2 = !{!3, !4, !5}"
    , "!3 = !{i32 0, !\"air.buffer\", !\"air.location_index\", i32 0, i32 1, !\"air.read_write\", !\"air.address_space\", i32 1, !\"air.arg_type_size\", i32 4, !\"air.arg_type_align_size\", i32 4, !\"air.arg_type_name\", !\"int\", !\"air.arg_name\", !\"output\"}"
    , "!4 = !{i32 1, !\"air.buffer\", !\"air.buffer_size\", i32 4, !\"air.location_index\", i32 1, i32 1, !\"air.read\", !\"air.address_space\", i32 2, !\"air.arg_type_size\", i32 4, !\"air.arg_type_align_size\", i32 4, !\"air.arg_type_name\", !\"uint\", !\"air.arg_name\", !\"count\"}"
    , "!5 = !{i32 2, !\"air.thread_position_in_grid\", !\"air.arg_type_name\", !\"uint\"}"
    , "!6 = !{i32 2, i32 8, i32 0}"
    , "!7 = !{!\"Metal\", i32 4, i32 0, i32 0}"
    ]

stop :: String -> LLVM Metal a
stop msg = internalError string $ "accelerate-llvm-metal: " ++ msg

