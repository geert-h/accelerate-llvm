{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Array.Accelerate.LLVM.Metal.Execute () where

import Control.Concurrent.MVar (putMVar, readMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)

import Data.Array.Accelerate.AST.Idx (Idx)
import Data.Array.Accelerate.AST.Execute (Execute(..))
import Data.Array.Accelerate.AST.LeftHandSide
  ( LeftHandSide(..), lhsToTupR )
import Data.Array.Accelerate.AST.Schedule (IOFun)
import Data.Array.Accelerate.AST.Schedule.Uniform
  ( UniformScheduleFun(..), UniformSchedule(..), Effect(..) )
import Data.Array.Accelerate.AST.Var (Var(..))

import Data.Array.Accelerate.LLVM.Metal.State (evalMetal, defaultTarget)
import Data.Array.Accelerate.LLVM.Metal.Kernel (MetalKernel(..))
import Data.Array.Accelerate.LLVM.Metal.Execute.Environment
import Data.Array.Accelerate.LLVM.Metal.Execute.Binding (executeBinding)
import Data.Array.Accelerate.LLVM.Metal.Execute.Marshal (baseToValues, MarshalledKernel(..), marshalKernel)
import Data.Array.Accelerate.LLVM.Metal.Execute.Par (Par, evalPar, liftPar, spawnPar)
import Data.Array.Accelerate.LLVM.Metal.Execute.Generate (launchGenerateI32)
import Data.Array.Accelerate.Error (internalError)
import Formatting

instance Execute UniformScheduleFun MetalKernel where
  data Linked UniformScheduleFun MetalKernel t =
    MetalLinked (UniformScheduleFun MetalKernel () t)

  linkAfunSchedule = MetalLinked

  executeAfunSchedule _ (MetalLinked function) =
    executeFun function

executeFun :: UniformScheduleFun MetalKernel () f -> IOFun f
executeFun (Sbody body) = evalMetal defaultTarget $ evalPar $ execute Empty body

executeFun (Slam lhs1 (Slam lhs2 function)) =
  curry (executeFun (Slam (LeftHandSidePair lhs1 lhs2) function))

executeFun (Slam lhs (Sbody body)) = \arguments ->
  evalMetal defaultTarget $ evalPar $ do
    values <- liftIO $ baseToValues (lhsToTupR lhs) arguments
    execute (push' Empty (lhs, values)) body

execute :: Gamma env -> UniformSchedule MetalKernel env -> Par ()
execute env = \case
  Return -> pure ()

  Alet lhs binding next -> do
    value <- liftPar $ executeBinding env (lhsToTupR lhs) binding
    execute (push' env (lhs, value)) next

  Effect effect next -> do
    executeEffect env effect
    execute env next

  Acond (Var _ idx) yes no next -> do
    case prj' idx env of
      ValueScalar _ condition ->
        execute env (if condition == 1 then yes else no)
    execute env next

  Spawn l r -> do
    spawnPar (execute env l)
    execute env r

  _ ->
    unsupported "schedule loop execution is not implemented"

executeEffect :: Gamma env -> Effect MetalKernel env -> Par ()
executeEffect env = \case
  SignalAwait signals ->
    forM_ signals $ \idx ->
      case prj' idx env of
        ValueSignal signal -> liftIO $ readMVar signal
        _ -> unsupported "expected signal"

  SignalResolve signals ->
    forM_ signals $ \idx ->
      case prj' idx env of
        ValueSignalResolver signal -> liftIO $ putMVar signal ()
        _ -> unsupported "expected signal resolver"

  RefWrite (Var _ refIdx) (Var _ valueIdx) ->
    case prj' refIdx env of
      ValueOutputRef write ->
        liftIO $ write (prj' valueIdx env)
      _ ->
        unsupported "expected output reference"

  Exec _ function arguments -> do
    marshalled <- liftIO $ marshalKernel env function arguments
    case marshalled of
      MarshalledKernel kernel kernelEnv ->
        launchScheduledKernel kernel kernelEnv

  _ ->
    unsupported "assertion/trace execution is not implemented"

launchScheduledKernel :: MetalKernel env -> Gamma env -> Par ()
launchScheduledKernel kernel env = do
  dims <- mapM (readDimension env) (kernelElements kernel)
  let count = product dims
      limit = min (toInteger (maxBound :: Int)) (toInteger (maxBound :: Word32))

  if count > limit
    then unsupported "Generate element count exceeds launch limits"
    else
      case prj' (kernelOutput kernel) env of
        ValueBuffer _ output ->
          liftIO $
            launchGenerateI32 (kernelMain kernel) output (fromInteger count)
        _ -> unsupported "expected Generate output buffer"

readDimension :: Gamma env -> Idx env Int -> Par Integer
readDimension env idx =
  let ValueScalar _ n = prj' idx env
  in pure (max 0 (toInteger n))

unsupported :: String -> Par a
unsupported message =
  internalError string ("accelerate-llvm-metal: " ++ message)
