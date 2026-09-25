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
import Data.Array.Accelerate.AST.LeftHandSide (LeftHandSide(..), Exists (..))
import Data.Array.Accelerate.AST.Partitioned
import Data.Array.Accelerate.Representation.Array (ArrayR(..))
import Data.Array.Accelerate.Representation.Shape (ShapeR(..), DIM1)
import Data.Array.Accelerate.Representation.Type (TupR(..))
import Data.Array.Accelerate.Type (scalarTypeInt32, scalarTypeInt)

import Data.Array.Accelerate.LLVM.State (LLVM)
import Data.Array.Accelerate.LLVM.Metal.Operation (MetalOp(..))
import Data.Array.Accelerate.LLVM.Metal.Target (Metal, metalDataLayout, metalTargetTriple)
import Data.Array.Accelerate.Error (internalError)
import Formatting (string)

import Data.List (intercalate)
import qualified Data.ByteString.Short.Char8 as SBS
import Data.Array.Accelerate.LLVM.CodeGen.Cluster (OpCodeGen)
import Data.Array.Accelerate.LLVM.CodeGen.Default (defaultCodeGenPermuteUnique, defaultCodeGenFold, defaultCodeGenFold1, defaultCodeGenScan1, defaultCodeGenScan', defaultCodeGenScan, defaultCodeGenPermute, defaultCodeGenBackpermute, defaultCodeGenMap, defaultCodeGenGenerate)
import Data.Array.Accelerate.LLVM.CodeGen.IR (Operands(..))

data MetalCode env = MetalCode
  { metalCodeSource   :: !String
  , metalCodeName     :: !String
  , metalCodeElements :: ![Idx env Int]
  , metalCodeOutput   :: !(Idx env (Buffer Int32))
  }

codegen :: String -> Env AccessGroundR env -> Clustered MetalOp args -> Args env args -> LLVM Metal (MetalCode env)
codegen name parameterTypes cluster args = codegenIndependent name parameterTypes (toFlatClustered cluster args)

-- codegen :: String
--         -> Env AccessGroundR env
--         -> Clustered MetalOp args
--         -> Args env args
--         -> LLVM Metal
--            ( Int -- The size of the kernel data, shared by all threads working on this kernel.
--            , Module (KernelType env))
-- codegen name env cluster args
--  | flat@(FlatCluster shr idxLHS sizes dirs localR localLHS flatOps) <- toFlatClustered cluster args
--  , parallelDepth <- flatClusterIndependentLoopDepth flat
--  , Exists parallelShr <- shapeRFromRank parallelDepth =
--   codeGenFunction linkage name type' (LLVM.Lam argTp "arg" . LLVM.Lam primType "locks_array" . LLVM.Lam primType "thread.index" . LLVM.Lam primType "thread.count") $ do
--     extractEnv
--
--     -- Before the parallel work of a kernel is started, we first run the function once.
--     -- This first call will initialize kernel memory (SEE: Kernel Memory)
--     -- and decide whether the runtime may try to let multiple threads work on this kernel.
--     initBlock <- newBlock "init"
--     finishBlock <- newBlock "finish" -- Finish function from the work assisting paper
--     workBlock <- newBlock "work"
--     _ <- switch (OP_Word32 threadIndex) workBlock [(0xFFFFFFFF, initBlock), (0xFFFFFFFE, finishBlock)]
--     let hasPermute = hasNPermute flat
--
--     -- Parallelise over all independent dimensions
--     let (envs, loops) = initEnv gamma shr idxLHS sizes dirs localR localLHS
--
--     -- If we parallelize over all dimensions, choose a large tile size.
--     -- The work per iteration is probably very small.
--     -- If we do not parallelize over all dimensions, choose a tile size of 1.
--     -- The work per iteration is probably large enough.
--     let tileSize = if parallelDepth == rank shr then chunkSize parallelShr else chunkSizeOne parallelShr
--     let parSizes = parallelIterSize parallelShr loops
--
--     setBlock initBlock
--     do
--       tileCount <- chunkCount parallelShr parSizes (A.lift (shapeType parallelShr) tileSize)
--       tileCount' <- shapeSize parallelShr tileCount
--       -- We are not using kernel memory, so no need to initialize it.
--
--       -- Assert that there are at most 2^47 tiles
--       A.when (A.gt singleType tileCount' $ A.liftInt (1 `shiftL` 47)) $
--         trapWithMessage "Accelerate: Parallel loops must have at most 2^47 tiles"
--
--       OP_Bool isSmall <- A.lt singleType tileCount' $ A.liftInt 2
--       value <- instr' $ LLVM.Select isSmall (scalar (scalarType @Word8) 0) (scalar scalarType 1)
--       retval_ value
--
--     setBlock finishBlock
--     -- Nothing has to be done in the finish function for this kernel.
--     retval_ $ scalar (scalarType @Word8) 0
--
--     setBlock workBlock
--     let ann =
--           if parallelDepth /= rank shr then []
--           else {- if hasPermute then -} [Loop.LoopInterleave]
--           -- else [Loop.LoopVectorize]
--     workassistChunked ann parallelShr workassistIndex workPerThread 1024 threadIndex threadCount tileSize parSizes $ \idx -> do
--       let envs' = envs{
--           envsLoopDepth = parallelDepth,
--           envsIdx =
--             foldr (uncurry Env.partialUpdate) (envsIdx envs)
--             $ zip (shapeOperandsToList parallelShr idx) (map (\(i, _, _) -> i) loops),
--           -- Independent operations should not depend on envsIsFirst.
--           envsIsFirst = OP_Bool $ boolean False,
--           envsDescending = False
--         }
--       genSequential envs' (drop parallelDepth loops) $ opCodeGens opCodeGen flatOps
--
--     pure 0
--   where
--     (argTp, extractEnv, workassistIndex, workPerThread, threadIndex {- or flag -}, threadCount, kernelMem', gamma) = bindHeaderEnv env
--
--     isDescending :: LoopDirection Int -> Bool
--     isDescending LoopDescending = True
--     isDescending _ = False


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

-- opCodeGen :: FlatOp MetalOp env idxEnv -> (LoopDepth, OpCodeGen Metal MetalOp env idxEnv)
-- opCodeGen flatOp@(FlatOp op args idxArgs) = case op of
--   MetalGenerate -> defaultCodeGenGenerate args idxArgs
--   MetalMap -> defaultCodeGenMap args idxArgs
--   MetalBackpermute -> defaultCodeGenBackpermute args idxArgs
--   -- TODO: Similar to Native, we should use one global array of locks, instead of an array per permute
--   MetalPermute
--     | combineFun :>: output :>: locks :>: source :>: _ <- args
--     , i1 :>: i2 :>: _ :>: i3 :>: _ <- idxArgs ->
--       defaultCodeGenPermute
--         (\envs j _ -> atomically envs locks $ OP_Int j)
--         (combineFun :>: output :>: source :>: ArgsNil)
--         (i1 :>: i2 :>: i3 :>: ArgsNil)
--   MetalPermute' -> defaultCodeGenPermuteUnique args idxArgs
--   MetalFold -> defaultCodeGenFold flatOp args idxArgs
--   MetalFold1 -> defaultCodeGenFold1 flatOp args idxArgs
--   MetalScan1 dir -> defaultCodeGenScan1 dir flatOp args idxArgs
--   MetalScan' dir -> defaultCodeGenScan' dir flatOp args idxArgs
--   MetalScan dir -> defaultCodeGenScan dir flatOp args idxArgs

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
    [ "target triple = " ++ show (SBS.unpack metalTargetTriple)
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

