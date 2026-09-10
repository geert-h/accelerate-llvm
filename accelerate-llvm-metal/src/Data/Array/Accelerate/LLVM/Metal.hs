{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Array.Accelerate.LLVM.Metal (

  Acc, Arrays,
  Afunction, AfunctionR,

  -- * Synchronous execution
  run, --runWith,
  run1, --run1With,
  runN, --runNWith,
  {- stream, streamWith,

  -- * Asynchronous execution
  Async,
  wait, poll, cancel,

  runAsync, runAsyncWith,
  run1Async, run1AsyncWith,
  runNAsync, runNAsyncWith,

  -- * Ahead-of-time compilation
  runQ, runQWith,
  runQAsync, runQAsyncWith, -}

  -- * Execution targets
  Metal, -- createTargetForDevice, createTargetFromContext,

  -- * Controlling host-side allocation
  -- registerPinnedAllocatorWith,

  MetalOp, MetalKernel
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Backend
import Data.Array.Accelerate.Trafo.Sharing
import Data.Array.Accelerate.AST.Schedule.Uniform

import Data.Array.Accelerate.LLVM.Metal.Target
import Data.Array.Accelerate.LLVM.Metal.Operation
import Data.Array.Accelerate.LLVM.Metal.Kernel
import Data.Array.Accelerate.LLVM.Metal.Execute ()

instance Backend Metal where
  type Schedule Metal = UniformScheduleFun
  type Kernel Metal = MetalKernel

run :: Arrays a => Acc a -> a
run = runAt @Metal

run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
run1 = run1At @Metal

runN :: Afunction f => f -> AfunctionR f
runN = runNAt @Metal

