{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.LLVM.Metal.Execute.Binding
  ( executeBinding
  ) where

import Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.AST.Operation (ArrayInstr(..))
import Data.Array.Accelerate.AST.Schedule.Uniform
  ( Binding(..), BaseR(..), BasesR )
import Data.Array.Accelerate.AST.Var (Var(..))
import Data.Array.Accelerate.Interpreter (evalExp, EvalArrayInstr(..))
import Data.Array.Accelerate.Representation.Elt (scalarTypeSize)
import Data.Array.Accelerate.Representation.Ground (GroundR(..))
import Data.Array.Accelerate.Representation.Shape (ShapeR(..))
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.State (LLVM)
import Data.Array.Accelerate.LLVM.Metal.Array.Prim (mallocBuffer)
import Data.Array.Accelerate.LLVM.Metal.Execute.Environment
import Data.Array.Accelerate.LLVM.Metal.Target (Metal, metalContext)
import Formatting (string)
import Data.Array.Accelerate.Error (internalError)

executeBinding :: Gamma env -> BasesR t -> Binding env t -> LLVM Metal (Distribute Value t)
executeBinding env baseTp = \case
  Compute expression -> do
    result <- evalExp expression (evalArrayInstr env)
    pure $ scalarToValues (mapTupR expectScalarType baseTp) result
  Alloc shr tp shape -> do
    let count = shapeElements shr (prjVars shape env)
        bytes = count * toInteger (scalarTypeSize tp)

    if bytes > toInteger (maxBound :: Int)
      then unsupported "allocation size exceeds host Int"
      else do
        context <- asks metalContext
        buffer <- liftIO $ mallocBuffer context (fromInteger bytes)
        pure (ValueBuffer tp buffer)
  NewSignal _ -> do
    signal <- liftIO newEmptyMVar
    pure (ValueSignal signal, ValueSignalResolver signal)

  NewRef _ -> do
    ref <- liftIO newEmptyMVar
    pure (ValueRef ref, ValueOutputRef (putMVar ref))

  RefRead (Var _ idx) ->
    case prj' idx env of
      ValueRef ref -> do
        value <- liftIO $ readMVar ref
        case reprIsSingle @Value @_ @Value value of
          Refl -> pure value
      _ -> unsupported "expected a reference"

  Use {} -> unsupported "host-to-device input transfer is not implemented"
  Unit _ -> unsupported "scalar-to-buffer transfer is not implemented"

expectScalarType :: BaseR t -> ScalarType t
expectScalarType (BaseRground (GroundRscalar tp)) = tp
expectScalarType _ = error "accelerate-llvm-metal: Compute expected sclar types"

shapeElements :: ShapeR sh -> Distribute Value sh -> Integer
shapeElements ShapeRz _ = 1
shapeElements (ShapeRsnoc shr) (sh, ValueScalar _ n)
  | n <= 0 = 0
  | otherwise = shapeElements shr sh * toInteger n

evalArrayInstr :: Gamma env -> EvalArrayInstr (LLVM Metal) (ArrayInstr env)
evalArrayInstr env = EvalArrayInstr $ \instruction _ ->
  case instruction of
    Parameter (Var _ idx) ->
      case prj' idx env of
        ValueScalar _ value -> pure value
        _ -> unsupported "expected a scalar parameter"
    Index _ -> unsupported "host-side indexing of device is not implemented"

unsupported :: String -> LLVM Metal a
unsupported message =
  internalError string $ "accelerate-llvm-metal: " ++ message

