
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Array.Accelerate.LLVM.Metal.Foreign (

  ForeignExp(..),

  -- useful re-exports
  LLVM,
  Metal(..),
  liftIO,

) where

import qualified Data.Array.Accelerate.Sugar.Foreign                as S

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.Metal.Target

import Control.Monad.State
import Data.Typeable


instance CompileForeignExp Metal where
  foreignExp (ff :: asm (x -> y))
    | Just Refl        <- eqT @asm @ForeignExp
    , ForeignExp _ asm <- ff = Just asm
    | otherwise              = Nothing

instance S.Foreign ForeignExp where
  strForeign (ForeignExp s _) = s


data ForeignExp f where
  ForeignExp :: String
             -> IRFun1 Metal (x -> y)
             -> ForeignExp (x -> y)

deriving instance Typeable ForeignExp
