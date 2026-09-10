{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.LLVM.Metal.Execute.Environment
  ( module Data.Array.Accelerate.AST.Environment
  , Gamma
  , Value(..)
  , scalarToValues
  ) where

import Control.Concurrent.MVar (MVar)

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Buffer (Buffer)
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.AST.Schedule.Uniform (Signal, SignalResolver, Ref, OutputRef)
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Metal.Array.Data (MetalBuffer)

type Gamma = Env Value

data Value t where
  ValueScalar :: !(ScalarType t) -> !t -> Value t

  ValueBuffer :: !(ScalarType t) -> !MetalBuffer -> Value (Buffer t)

  ValueSignal :: !(MVar ()) -> Value Signal

  ValueSignalResolver :: !(MVar ()) -> Value SignalResolver

  ValueRef :: !(MVar (Value t)) -> Value (Ref t)

  ValueOutputRef :: !(Value t -> IO ()) -> Value (OutputRef t)

instance Distributes Value where
  reprIsSingle (ValueScalar tp _) = reprIsSingle tp
  reprIsSingle (ValueBuffer _ _) = Refl
  reprIsSingle (ValueSignal _) = Refl
  reprIsSingle (ValueSignalResolver _) = Refl
  reprIsSingle (ValueRef _) = Refl
  reprIsSingle (ValueOutputRef _) = Refl

  pairImpossible (ValueScalar tp _) = pairImpossible tp
  unitImpossible (ValueScalar tp _) = unitImpossible tp

scalarToValues :: TypeR tp -> tp -> Distribute Value tp
scalarToValues TupRunit _ = ()
scalarToValues (TupRpair t1 t2) (v1, v2) = (scalarToValues t1 v1, scalarToValues t2 v2)
scalarToValues (TupRsingle tp) value
  | Refl <- reprIsSingle @ScalarType @_ @Value tp
  = ValueScalar tp value
