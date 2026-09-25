{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.LLVM.Metal.CodeGen
  ( MetalCode(..)
  , codegen
  ) where

import Data.Type.Equality ((:~:)(Refl))

import qualified Data.Array.Accelerate.AST.Environment as Env
import Data.Array.Accelerate.LLVM.Metal.Foreign
import Data.Array.Accelerate.AST.Idx (Idx)
import Data.Array.Accelerate.AST.LeftHandSide (Exists (..), flattenTupR)
import Data.Array.Accelerate.AST.Partitioned
import Data.Array.Accelerate.Representation.Type (TupR(..), TupleIdx (..))

import Data.Array.Accelerate.LLVM.Metal.Operation (MetalOp(..))
import Data.Array.Accelerate.Error (internalError)

import qualified Data.ByteString.Short.Char8 as SBS
import Data.Array.Accelerate.LLVM.CodeGen.Cluster (OpCodeGen, genSequential, opCodeGens)
import Data.Array.Accelerate.LLVM.CodeGen.Default (defaultCodeGenPermuteUnique, defaultCodeGenFold, defaultCodeGenFold1, defaultCodeGenScan1, defaultCodeGenScan', defaultCodeGenScan, defaultCodeGenBackpermute, defaultCodeGenMap, defaultCodeGenGenerate)
import Data.Array.Accelerate.LLVM.CodeGen.IR (Operands(..))

import qualified LLVM.AST.Type.Function as LLVM
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import LLVM.AST.Type.Representation
import LLVM.AST.Type.Module
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic as A
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.Metal.CodeGen.Base
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Metadata (MetadataNodeID, Metadata (..), MetadataNode (..))
import LLVM.AST.Type.Constant (Constant(ScalarConstant))
import LLVM.AST.Type.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.String
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Volatile
import Data.Array.Accelerate.Representation.Shape (shapeRFromRank)

data MetalCode env = MetalCode
  { metalCodeElements :: ![Idx env Int]
  , metalCodeWork     :: Module (KernelType env)
  }

codegen :: forall env args.
           String
        -> Env AccessGroundR env
        -> Clustered MetalOp args
        -> Args env args
        -> LLVM Metal (MetalCode env)
codegen name env cluster args
  | Refl <- marshalFunResultUnit env =
    codegenIndependent name env flat independentLoopDepth
  where
    flat = toFlatClustered cluster args
    independentLoopDepth = flatClusterIndependentLoopDepth flat

codegenIndependent
  :: forall env.
     LLVM.Result (MarshalFun env) ~ ()
  => String
  -> Env AccessGroundR env
  -> FlatCluster MetalOp env
  -> Int
  -> LLVM Metal (MetalCode env)
codegenIndependent name env flatCluster parallelDepth
  | FlatCluster shr idxLHS sizes dirs localR localLHS flatOps <- flatCluster
  , Exists parallelShr <- shapeRFromRank parallelDepth
  , (gamma, makeKernel') <- makeKernel name env = do
    kernelWork <- makeKernel' "" $ do
      let (envs, loops) = initEnv gamma shr idxLHS sizes dirs localR localLHS
          parSizes = parallelIterSize parallelShr loops
          threadId = OP_Word32 (LocalReference (PrimType primType) "thread_id")
      parSize <- shapeSize parallelShr parSizes
      linearIdx <- A.fromIntegral TypeWord32 numType threadId

      A.when (A.lt singleType linearIdx parSize) $ do
        idx <- indexOfInt parallelShr parSizes linearIdx
        let envs' = envs
             { envsLoopDepth = parallelDepth
             , envsIdx =
                 foldr (uncurry Env.partialUpdate) (envsIdx envs)
                 $ zip (shapeOperandsToList parallelShr idx) (map (\(i, _, _) -> i) loops)
             , envsIsFirst = OP_Bool $ boolean False
             , envsDescending = False
             }
        genSequential envs' (drop parallelDepth loops) $ opCodeGens opCodeGen flatOps

      return_

    return $ MetalCode
      (take parallelDepth $ map sizeVar $ flattenTupR sizes)
      kernelWork

sizeVar :: Exists (Var GroundR env) -> Idx env Int
sizeVar (Exists (Var (GroundRscalar (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))) idx))
  = idx
sizeVar _ = internalError "Expected Int variable"

-- Generates code for a Metal module.
makeKernel
  :: LLVM.Result (MarshalFun env) ~ ()
  => String
  -> Env AccessGroundR env
  -> (Gamma env, String -> CodeGen Metal () -> LLVM Metal (Module (KernelType env)))
makeKernel name env =
  let (envType, extractEnv, gamma) = bindMetalEnvFromStruct env in
  ( gamma
  , \postfix body ->
    snd <$> codeGenKernel
      (name ++ postfix)
      (LLVM.Lam (PtrPrimType envType (AddrSpace 2)) "env" . LLVM.Lam primType "thread_id")
      (do typedef "Env" (downcast (skipTypeAlias envType)) >> extractEnv >> body)
      (descEnvTypes env envType)
  )

metalFieldType :: AccessGroundR t -> PrimType (MarshalStorageArg t)
metalFieldType (AccessGroundRscalar tp)
  | Refl <- marshalScalarArg tp = bufferEltR tp
metalFieldType (AccessGroundRbuffer _ tp) = PtrPrimType (bufferEltR tp) (AddrSpace 1)

metalEnvFields :: Env AccessGroundR env -> TupR PrimType (MarshalEnv env)
metalEnvFields Empty = TupRunit
metalEnvFields (Push env arg) = TupRpair (metalEnvFields env) (TupRsingle (metalFieldType arg))

-- TODO: it is maybe not such a good idea to replace the bindEnvFromStruct call
-- with a purpose built bindMetalEnvFromStruct for scalability purposes
bindMetalEnvFromStruct
  :: forall env. Env AccessGroundR env
  -> (PrimType (Struct (MarshalEnv env)), CodeGen Metal (), Gamma env)
bindMetalEnvFromStruct e =
  let (gen, gamma, _, bfrcnt) = go id e
  in (envTp, do declareAliasScopes bfrcnt >> gen, gamma)
  where
    envTp = NamedPrimType "Env" $ StructPrimType False $ metalEnvFields e
    operandEnv = LocalReference (PrimType (PtrPrimType envTp (AddrSpace 2))) "env"
    go :: forall env'.
          (forall t.
            TupleIdx (MarshalEnv env') t
            -> TupleIdx (MarshalEnv env) t)
       -> Env AccessGroundR env'
       -> (CodeGen Metal () -- Gets names of scopes as argument
          , Gamma env'
          , Int -- Next fresh scalar variable index
          , Int -- Next fresh buffer variable index
          )
    go _ Empty = (return (), Empty, 0, 0)
    go toTupleIdx (Push env (AccessGroundRscalar tp@(SingleScalarType t)))
      | Refl <- marshalScalarArg tp
      , Refl <- singleTypeBufferEltR t =
        ( do
            instr_ $ downcast $
              namePtr := GetElementPtr (gepStruct (bufferEltR tp) operandEnv $ toTupleIdx $ TupleIdxRight TupleIdxSelf)
            instr_ $ downcast $ name := Load NonVolatile operandPtr Nothing
            cg
        , gamma `Push` GroundOperandParam operand
        , sclrcnt + 1
        , bfrcnt
        )
      where
        (cg, gamma, sclrcnt, bfrcnt) = go (toTupleIdx . TupleIdxLeft) env
        operand    = LocalReference (PrimType $ ScalarPrimType tp) name
        operandPtr = LocalReference (PrimType $ PtrPrimType (bufferEltR tp) (AddrSpace 2)) namePtr
        name = fromString $ "param." ++ show sclrcnt
        namePtr = fromString $ "param." ++ show sclrcnt ++ ".ptr"

    go _ (Push _ (AccessGroundRscalar (VectorScalarType _))) =
       internalError "Metal vector-valued scalar arguments are not implemented"

    go toTupleIdx (Push env (AccessGroundRbuffer m (tp :: ScalarType t))) =
      ( do
        instr_ (downcast $
          namePtr := GetElementPtr (gepStruct ptrTp operandEnv $ toTupleIdx $ TupleIdxRight TupleIdxSelf)
          )
        instr_ (downcast $ name := Load NonVolatile operandPtr Nothing)
        cg
      , gamma `Push` GroundOperandBuffer irBuffer
      , sclrcnt
      , bfrcnt + 1
      )
      where
        (cg, gamma, sclrcnt, bfrcnt) = go (toTupleIdx . TupleIdxLeft) env
        ptrTp = PtrPrimType (bufferEltR tp) (AddrSpace 1)
        operand = LocalReference (PrimType ptrTp) name
        operandPtr = LocalReference (PrimType $ PtrPrimType ptrTp (AddrSpace 2)) namePtr
        prefix = case m of
          In  -> "in."
          Out -> "out."
          Mut -> "mut."
        name = fromString $ prefix ++ show bfrcnt
        namePtr = fromString $ prefix ++ show sclrcnt ++ ".ptr"

        bfrcnt' = case m of
          In -> bfrcnt
          _  -> bfrcnt + 1

        alias = case m of
          In -> Just (3, 4)
          _  -> Just (3 * bfrcnt' + 6, 3 * bfrcnt' + 7)

        irBuffer = IRBuffer operand (AddrSpace 1) NonVolatile IRBufferScopeArray alias

descEnvTypes ::Env AccessGroundR env -> PrimType (Struct (MarshalEnv env)) -> CodeGen Metal [MetadataNodeID]
descEnvTypes env envTp = do
    innerBuff <- buildInnerArgBuff env

    let (sz, al) = primSizeAlignment envTp

    envDesc <- addMetadata $ const $ map Just
      [ MetadataConstantOperand $ ScalarConstant scalarTypeInt32 0
      , MetadataStringOperand "air.indirect_buffer"
      , MetadataStringOperand "air.buffer_size"
      , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral sz
      , MetadataStringOperand "air.location_index"
      , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 0
      , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 1
      , MetadataStringOperand "air.read"
      , MetadataStringOperand "air.address_space"
      , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 2
      , MetadataStringOperand "air.struct_type_info"
      , MetadataNodeOperand $ MetadataNodeReference innerBuff
      , MetadataStringOperand "air.arg_type_size"
      , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral sz
      , MetadataStringOperand "air.arg_type_align_size"
      , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral al
      , MetadataStringOperand "air.arg_type_name"
      , MetadataStringOperand "Env"
      , MetadataStringOperand "air.arg_name"
      , MetadataStringOperand "env"
      ]

    threadDesc <- addMetadata $ const
      [ Just $ MetadataConstantOperand $ ScalarConstant scalarTypeInt32 1 -- second func param
      , Just $ MetadataStringOperand "air.thread_position_in_grid"
      , Just $ MetadataStringOperand "air.arg_type_name"
      , Just $ MetadataStringOperand "uint"
      ]

    pure [envDesc, threadDesc]

buildInnerArgBuff :: Env AccessGroundR env -> CodeGen Metal MetadataNodeID
buildInnerArgBuff env = do
  (_, _, fields) <- buildEnvDesc env
  addMetadata $ const $ map Just fields

buildEnvDesc :: Env AccessGroundR env -> CodeGen Metal (Int, Int, [Metadata])
buildEnvDesc Empty = pure (0, 0, [])
buildEnvDesc (Push env arg@(AccessGroundRscalar t)) = do
  (n, cursor, r) <- buildEnvDesc env
  let (sz, al) = primSizeAlignment (metalFieldType arg)
      offset   = makeAligned cursor al

  d <- addMetadata $ const $ map Just
    [ MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral n
    , MetadataStringOperand "air.indirect_constant"
    , MetadataStringOperand "air.location_index"
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral n
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 1
    , MetadataStringOperand "air.arg_type_name"
    , MetadataStringOperand $ SBS.pack $ metalTypeName t
    , MetadataStringOperand "air.arg_name"
    , MetadataStringOperand $ SBS.pack $ "field" ++ show n
    ]
  pure (n + 1, offset + sz,
    r ++
    [ MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral offset
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral sz
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 0
    , MetadataStringOperand $ SBS.pack $ metalTypeName t
    , MetadataStringOperand $ SBS.pack $ "field" ++ show n
    , MetadataStringOperand "air.indirect_argument"
    , MetadataNodeOperand $ MetadataNodeReference d
    ])
buildEnvDesc (Push env arg@(AccessGroundRbuffer m t)) = do
  (n, cursor, r) <- buildEnvDesc env

  let (sz, al)   = primSizeAlignment (metalFieldType arg)
      offset     = makeAligned cursor al
      (sz', al') = primSizeAlignment (bufferEltR t)

  d <- addMetadata $ const $ map Just
    [ MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral n
    , MetadataStringOperand "air.buffer"
    , MetadataStringOperand "air.location_index"
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral n
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 1
    , MetadataStringOperand $
        case m of
          In -> "air.read"
          Out  -> "air.write"
          Mut  -> "air.read_write"
    , MetadataStringOperand "air.address_space"
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 1
    , MetadataStringOperand "air.arg_type_size"
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral sz'
    , MetadataStringOperand "air.arg_type_align_size"
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral al'
    , MetadataStringOperand "air.arg_type_name"
    , MetadataStringOperand $ SBS.pack $ metalTypeName t
    , MetadataStringOperand "air.arg_name"
    , MetadataStringOperand $ SBS.pack $ "field" ++ show n
    ]
  pure (n + 1, offset + sz,
    r ++
    [ MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral offset
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 $ fromIntegral sz
    , MetadataConstantOperand $ ScalarConstant scalarTypeInt32 0
    , MetadataStringOperand $ SBS.pack $ metalTypeName t
    , MetadataStringOperand $ SBS.pack $ "field" ++ show n
    , MetadataStringOperand "air.indirect_argument"
    , MetadataNodeOperand $ MetadataNodeReference d
    ])

opCodeGen
  :: FlatOp MetalOp env idxEnv
  -> (LoopDepth, OpCodeGen Metal MetalOp env idxEnv)
opCodeGen flatOp@(FlatOp op args idxArgs) = case op of
  MetalGenerate -> defaultCodeGenGenerate args idxArgs
  MetalMap -> defaultCodeGenMap args idxArgs
  MetalBackpermute -> defaultCodeGenBackpermute args idxArgs
  MetalPermute' -> defaultCodeGenPermuteUnique args idxArgs
  MetalFold -> defaultCodeGenFold flatOp args idxArgs
  MetalFold1 -> defaultCodeGenFold1 flatOp args idxArgs
  MetalScan1 dir -> defaultCodeGenScan1 dir flatOp args idxArgs
  MetalScan' dir -> defaultCodeGenScan' dir flatOp args idxArgs
  MetalScan dir -> defaultCodeGenScan dir flatOp args idxArgs
  _ -> internalError "opCodeGen: function not supported yet"
  -- -- TODO: Similar to Native, we should use one global array of locks, instead of an array per permute
  -- MetalPermute
  --   | combineFun :>: output :>: locks :>: source :>: _ <- args
  --   , i1 :>: i2 :>: _ :>: i3 :>: _ <- idxArgs ->
  --     defaultCodeGenPermute
  --       (\envs j _ -> atomically envs locks $ OP_Int j)
  --       (combineFun :>: output :>: source :>: ArgsNil)
  --       (i1 :>: i2 :>: i3 :>: ArgsNil)



-- Helper function for converting types to metal accepted names
-- See Chapter on Data Types in the language specs:
-- https://developer.apple.com/metal/Metal-Shading-Language-Specification.pdf
metalTypeName :: ScalarType t -> String
metalTypeName (SingleScalarType (NumSingleType (IntegralNumType t))) = case t of
  TypeInt     -> "long"
  TypeInt8    -> "char"
  TypeInt16   -> "short"
  TypeInt32   -> "int"
  TypeInt64   -> "long"
  TypeWord    -> "ulong"
  TypeWord8   -> "uchar"
  TypeWord16  -> "ushort"
  TypeWord32  -> "uint"
  TypeWord64  -> "ulong"
metalTypeName (SingleScalarType (NumSingleType (FloatingNumType t))) = case t of
  TypeHalf   -> "half"
  TypeFloat  -> "float"
  TypeDouble -> internalError "Double not supported on Metal"
metalTypeName (VectorScalarType _) = internalError "vector argument metadata is not implemented for Metal"
