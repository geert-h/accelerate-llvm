{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TupleSections       #-}


module Data.Array.Accelerate.LLVM.Metal.Operation
  where

import Data.Array.Accelerate.AST.Exp
import Data.Array.Accelerate.AST.Operation
import Data.Array.Accelerate.AST.Partitioned
import Data.Array.Accelerate.Analysis.Hash.Exp
import Data.Array.Accelerate.Analysis.Hash.Operation
import Data.Array.Accelerate.Backend
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Graph
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Labels

import Data.Array.Accelerate.AST.Environment (weakenId )
import Data.Array.Accelerate.Representation.Array (ArrayR(..))
import Data.Array.Accelerate.Trafo.Var (DeclareVars(..), declareVars)
import Data.Array.Accelerate.Representation.Ground (buffersR)
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.Trafo.Operation.Bounds
import Data.Array.Accelerate.Trafo.Operation.Substitution (aletUnique, alet, weaken)
import Data.Array.Accelerate.Representation.Shape (ShapeR (..), shapeType, rank)
import Data.Array.Accelerate.Representation.Type (TypeR, TupR (..))
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Partitioning.ILP.ConstraintLanguage (Constraint(..))
import Data.Array.Accelerate.Trafo.Partitioning.ILP.LinearConstraint (Bounds, Number(..), Constants(..), Var(..), equal, lower)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (Solution)
import Lens.Micro
import Lens.Micro.Mtl
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Array.Accelerate.Trafo.Exp.Substitution

import Control.Monad.State.Strict

data MetalOp t where
  MetalMap       :: MetalOp (Fun' (s -> t)    -> In sh s -> Out sh  t -> ())
  MetalBackpermute :: MetalOp (Fun' (sh' -> sh) -> In sh t -> Out sh' t -> ())
  MetalGenerate  :: MetalOp (Fun' (sh -> t)              -> Out sh  t -> ())
  MetalPermute   :: MetalOp (Fun' (e -> e -> e)
                         -> Mut sh' e
                         -> Mut sh' Word32
                         -> In sh (PrimMaybe (sh', e))
                         -> ())
  MetalPermute'  :: MetalOp (Mut sh' e
                         -> In sh (PrimMaybe (sh', e))
                         -> ())
  MetalScan      :: Direction
               -> MetalOp (Fun' (e -> e -> e)
                         -> Exp' e
                         -> In (sh, Int) e
                         -> Out (sh, Int) e
                         -> ())
  MetalScan1     :: Direction
               -> MetalOp (Fun' (e -> e -> e)
                         -> In (sh, Int) e
                         -> Out (sh, Int) e
                         -> ())
  MetalScan'     :: Direction
               -> MetalOp (Fun' (e -> e -> e)
                         -> Exp' e
                         -> In (sh, Int) e
                         -> Out (sh, Int) e
                         -> Out sh e
                         -> ())
  MetalFold      :: MetalOp (Fun' (e -> e -> e)
                         -> Exp' e
                         -> In (sh, Int) e
                         -> Out sh e
                         -> ())
  MetalFold1     :: MetalOp (Fun' (e -> e -> e)
                         -> In (sh, Int) e
                         -> Out sh e
                         -> ())

instance PrettyOp MetalOp where
  prettyOp MetalMap         = "map"
  prettyOp MetalBackpermute = "backpermute"
  prettyOp MetalGenerate    = "generate"
  prettyOp MetalPermute     = "permute"
  prettyOp MetalPermute'    = "permuteUnique"
  prettyOp (MetalScan dir) = case dir of
    LeftToRight -> "scanl"
    RightToLeft -> "scanr"
  prettyOp (MetalScan1 dir) = case dir of
    LeftToRight -> "scanl1"
    RightToLeft -> "scanr1"
  prettyOp (MetalScan' dir) = case dir of
    LeftToRight -> "scanl'"
    RightToLeft -> "scanr'"
  prettyOp MetalFold        = "fold"
  prettyOp MetalFold1       = "fold1"

instance NFData' MetalOp where
  rnf' !_ = ()

instance OperationBounds MetalOp where
  boundsOptimizeOp = \case
    MetalMap -> boundsOptimizeMap
    MetalGenerate -> boundsOptimizeGenerate
    MetalBackpermute -> boundsOptimizeBackpermute
    MetalScan _ -> boundsOptimizeScan
    MetalScan1 _ -> boundsOptimizeScan1
    MetalScan' _ -> boundsOptimizeScan'
    MetalFold -> boundsOptimizeFold
    MetalFold1 -> boundsOptimizeFold1
    _ -> boundsOptimizeOpDefault

instance LowerAcc MetalOp where
  mkMap         a b c   = Exec MetalMap         (a :>: b :>: c :>:       ArgsNil)
  mkBackpermute a b c   = Exec MetalBackpermute (a :>: b :>: c :>:       ArgsNil)
  mkGenerate    a b     = Exec MetalGenerate    (a :>: b :>:             ArgsNil)
  mkScan dir f (Just seed) i@(ArgArray In (ArrayR shr ty) sh buf) o
    = Exec (MetalScan dir) (f :>: seed :>: i :>: o :>: ArgsNil)
  mkScan dir f Nothing i@(ArgArray In (ArrayR shr ty) sh buf) o
    = Exec (MetalScan1 dir) (f :>: i :>: o :>: ArgsNil)
  mkScan' dir f seed i@(ArgArray In (ArrayR shr ty) sh buf) o1 o2
    = Exec (MetalScan' dir) (f :>: seed :>: i :>: o1 :>: o2 :>: ArgsNil)
  mkPermute     (Just a) b@(ArgArray _ (ArrayR shr _) sh _) c
    | DeclareVars lhs w lock <- declareVars $ buffersR $ TupRsingle scalarTypeWord32
    = aletUnique lhs 
        (Alloc shr scalarTypeWord32 $ groundToExpVar (shapeType shr) sh)
        $ alet LeftHandSideUnit
          (Exec MetalGenerate ( -- TODO: The old pipeline used a 'memset 0' instead, which sounds faster...
                ArgFun (Lam (LeftHandSideWildcard (shapeType shr)) $ Body $ Const scalarTypeWord32 0)
            :>: ArgArray Out (ArrayR shr (TupRsingle scalarTypeWord32)) (weakenVars w sh) (lock weakenId) 
            :>: ArgsNil))
          (Exec MetalPermute (
                weaken w a 
            :>: weaken w b 
            :>: ArgArray Mut (ArrayR shr (TupRsingle scalarTypeWord32)) (weakenVars w sh) (lock weakenId) 
            :>: weaken w c 
            :>: ArgsNil))
  mkPermute Nothing a b = Exec MetalPermute' (a :>: b :>: ArgsNil)
  {-mkFold a (Just seed) b c = Exec MetalFold (a :>: seed :>: b :>: c :>: ArgsNil)
  mkFold a Nothing b c = Exec MetalFold1 (a :>: b :>: c :>: ArgsNil) -}

instance SimplifyOperation MetalOp where
  detectCopy MetalMap         = detectMapCopies
  detectCopy MetalBackpermute = detectBackpermuteCopies
  detectCopy _              = const []

instance SLVOperation MetalOp where
  slvOperation MetalGenerate    = defaultSlvGenerate    MetalGenerate
  slvOperation MetalMap         = defaultSlvMap         MetalMap
  slvOperation MetalBackpermute = defaultSlvBackpermute MetalBackpermute
  slvOperation _ = Nothing

instance EncodeOperation MetalOp where
  encodeOperation MetalMap         = intHost $(hashQ ("Map" :: String))
  encodeOperation MetalBackpermute = intHost $(hashQ ("Backpermute" :: String))
  encodeOperation MetalGenerate    = intHost $(hashQ ("Generate" :: String))
  encodeOperation MetalPermute     = intHost $(hashQ ("Permute" :: String))
  encodeOperation MetalPermute'    = intHost $(hashQ ("Permute'" :: String))
  encodeOperation (MetalScan LeftToRight)  = intHost $(hashQ ("Scanl" :: String))
  encodeOperation (MetalScan RightToLeft)  = intHost $(hashQ ("Scanr" :: String))
  encodeOperation (MetalScan1 LeftToRight) = intHost $(hashQ ("Scanl1" :: String))
  encodeOperation (MetalScan1 RightToLeft) = intHost $(hashQ ("Scanr1" :: String))
  encodeOperation (MetalScan' LeftToRight) = intHost $(hashQ ("Scanl'" :: String))
  encodeOperation (MetalScan' RightToLeft) = intHost $(hashQ ("Scanr'" :: String))
  encodeOperation MetalFold        = intHost $(hashQ ("Fold" :: String))
  encodeOperation MetalFold1       = intHost $(hashQ ("Fold1" :: String))

instance SetOpIndices MetalOp where
  setOpIndices _ MetalGenerate _ idxArgs = Just $ Right idxArgs -- Generate has no In arrays
  setOpIndices _ MetalMap _ (_ :>: _ :>: IdxArgIdx d i :>: ArgsNil)
    = Just $ Right $ IdxArgNone :>: IdxArgIdx d i :>: IdxArgIdx d i :>: ArgsNil
  setOpIndices _ MetalMap _ _ = error "Missing indices for MetalMap"
  setOpIndices _ MetalBackpermute _ _ = Just $ Left IsBackpermute
  setOpIndices _ (MetalScan _) _ (_ :>: _ :>: _ :>: IdxArgIdx d i :>: ArgsNil)
    -- Annotate the input with an index.
    -- Don't annotate the output. We don't fuse over the output of a normal scan,
    -- as the output of a scan is one longer than the input.
    -- We do fuse the other scans (scan' and scan1).
    = Just $ Right $ IdxArgNone :>: IdxArgNone :>: IdxArgIdx d i :>: IdxArgNone :>: ArgsNil
  setOpIndices _ (MetalScan _) _ _ = error "Missing indices for MetalScan"
  setOpIndices _ (MetalScan1 _) _ (_ :>: _ :>: IdxArgIdx d i :>: ArgsNil)
    = Just $ Right $ IdxArgNone :>: IdxArgIdx d i :>: IdxArgIdx d i :>: ArgsNil
  setOpIndices _ (MetalScan1 _) _ _ = error "Missing indices for MetalScan1"
  setOpIndices _ (MetalScan' _) _ (_ :>: _ :>: _ :>: IdxArgIdx d i :>: o :>: ArgsNil)
    = Just $ Right $ IdxArgNone :>: IdxArgNone :>: IdxArgIdx d i :>: IdxArgIdx d i :>: o :>: ArgsNil
  setOpIndices _ (MetalScan' _) _ _ = error "Missing indices for MetalScan'"
  setOpIndices indexVar MetalFold _ (_ :>: _ :>: _ :>: IdxArgIdx d i :>: ArgsNil)
    | Just i' <- indexVar d
    = Just $ Right $
      IdxArgNone :>: IdxArgNone :>: IdxArgIdx (d + 1) (i `TupRpair` TupRsingle (Var scalarTypeInt i')) :>: IdxArgIdx d i :>: ArgsNil
    | otherwise
    = Nothing
  setOpIndices _ MetalFold _ _ = error "Missing indices for MetalFold"
  setOpIndices indexVar MetalFold1 _ (_ :>: _ :>: IdxArgIdx d i :>: ArgsNil)
    | Just i' <- indexVar d
    = Just $ Right $
      IdxArgNone :>: IdxArgIdx (d + 1) (i `TupRpair` TupRsingle (Var scalarTypeInt i')) :>: IdxArgIdx d i :>: ArgsNil
    | otherwise
    = Nothing
  setOpIndices _ MetalFold1 _ _ = error "Missing indices for MetalFold1"
  setOpIndices indexVar MetalPermute (_ :>: _ :>: _ :>: ArgArray _ (ArrayR shr _) _ _ :>: _) (_ :: IdxArgs idxEnv f)
    | Just i <- findIndex shr
    = Just $ Right $
      IdxArgNone :>: IdxArgNone :>: IdxArgNone :>: IdxArgIdx (rank shr) i :>: ArgsNil
    | otherwise
    = Nothing
    where
      findIndex :: ShapeR sh -> Maybe (ExpVars idxEnv sh)
      findIndex ShapeRz = Just TupRunit
      findIndex (ShapeRsnoc shr')
        | Just a <- findIndex shr'
        , Just b <- indexVar (rank shr')
        = Just $ a `TupRpair` TupRsingle (Var scalarTypeInt b)
        | otherwise = Nothing
  setOpIndices indexVar MetalPermute' (_ :>: ArgArray _ (ArrayR shr _) _ _ :>: _) (_ :: IdxArgs idxEnv f)
    | Just i <- findIndex shr
    = Just $ Right $
      IdxArgNone :>: IdxArgIdx (rank shr) i :>: ArgsNil
    | otherwise
    = Nothing
    where
      findIndex :: ShapeR sh -> Maybe (ExpVars idxEnv sh)
      findIndex ShapeRz = Just TupRunit
      findIndex (ShapeRsnoc shr')
        | Just a <- findIndex shr'
        , Just b <- indexVar (rank shr')
        = Just $ a `TupRpair` TupRsingle (Var scalarTypeInt b)
        | otherwise = Nothing

  getOpLoopDirections (MetalScan dir) _ (_ :>: _ :>: IdxArgIdx _ i :>: _)
    | _ `TupRpair` TupRsingle var <- i = [(varIdx var, dir')]
    where
      dir' = case dir of
        LeftToRight -> LoopAscending
        RightToLeft -> LoopDescending
  getOpLoopDirections (MetalScan1 dir) _ (_ :>: _ :>: IdxArgIdx _ i :>: _)
    | _ `TupRpair` TupRsingle var <- i = [(varIdx var, dir')]
    where
      dir' = case dir of
        LeftToRight -> LoopAscending
        RightToLeft -> LoopDescending
  getOpLoopDirections (MetalScan' dir) _ (_ :>: _ :>: _ :>: IdxArgIdx _ i :>: _)
    | _ `TupRpair` TupRsingle var <- i = [(varIdx var, dir')]
    where
      dir' = case dir of
        LeftToRight -> LoopAscending
        RightToLeft -> LoopDescending
  getOpLoopDirections MetalFold _ (_ :>: _ :>: IdxArgIdx _ i :>: _)
    | _ `TupRpair` TupRsingle var <- i = [(varIdx var, LoopMonotone)]
  getOpLoopDirections MetalFold1 _ (_ :>: IdxArgIdx _ i :>: _)
    | _ `TupRpair` TupRsingle var <- i = [(varIdx var, LoopMonotone)]
  getOpLoopDirections _ _ _ = []

instance MakesILP MetalOp where
  type BackendArg MetalOp = Int -- direction: used to separate clusters later, preventing accidental horizontal fusion of backpermutes
  defaultBA = 0
  data BackendClusterArg MetalOp a = BCAN
  combineBackendClusterArg BCAN BCAN = BCAN

  mkGraph :: Node Comp
          -> MetalOp args
          -> LabelledArgs env args
          -> State (BackendGraphState MetalOp env) ()
  mkGraph c MetalBackpermute (_fun :>: L _ lIn :>: L _ lOut :>: ArgsNil) = do
    let bsIn  = getLabelArrDeps lIn
    let bsOut = getLabelArrDeps lOut
    wsIn <- use $ allWriters bsIn
    fusionILP.constraints %= (
      <> inputConstraints c wsIn
      <> [SameFoldSize c]
      <> [PinnedDirection c (map (,c) (S.toList bsIn)) []]
      <> [SameDirection [] (map (c,) (S.toList bsOut))])
    fusionILP.bounds %= (<> defaultBounds bsIn c bsOut)
    -- Different order, so no in-place paths.

  mkGraph c MetalGenerate (_fun :>: L _ lOut :>: ArgsNil) = do
    let bsOut = getLabelArrDeps lOut
    fusionILP.constraints %= (<> [SameDirection [] (map (c,) (S.toList bsOut))])
    fusionILP.bounds %= (<> defaultBounds mempty c bsOut)
    -- No input, so no in-place paths.

  mkGraph c MetalMap (L (ArgFun fun) _ :>: L _ lIn :>: L _ lOut :>: ArgsNil) = do
    let bsIn  = getLabelArrDeps lIn
    let bsOut = getLabelArrDeps lOut
    wsIn <- use $ allWriters $ getLabelArrDeps lIn
    fusionILP.constraints %= (
      <> inputConstraints c wsIn
      <> [SameFoldSize c]
      <> [SameDirection (map (,c) (S.toList bsIn)) (map (c,) (S.toList bsOut))])
    fusionILP.bounds %= (<> defaultBounds bsIn c bsOut)
    fusionILP.inplacePaths %= case isIdentity fun of
      Just Refl -> (<> mkUnitInplacePaths (Number nComps * Number nComps) c lIn lOut)
      _         -> (<> mkUnitInplacePaths 1 c lIn lOut)

  mkGraph c MetalPermute (_fun :>: L _ lTargets :>: L _ lLocks :>: L _ lIn :>: ArgsNil) = do
    let bsTargets = getLabelArrDeps lTargets
    let bsLocks   = getLabelArrDeps lLocks
    let bsIn      = getLabelArrDeps lIn
    wsTargets <- use $ allWriters bsTargets
    wsLocks   <- use $ allWriters bsLocks
    wsIn      <- use $ allWriters bsIn
    fusionILP %= (wsTargets <> wsLocks <> wsIn) `allBefore` c
    fusionILP.constraints %= (
      <> [SameFoldSize c])
    fusionILP.bounds %= (<> foldMap (equal (-2) . (`ReadDir` c)) (bsTargets <> bsLocks <> bsIn)
                         <> foldMap (equal (-3) . WriteDir c)    (bsTargets <> bsLocks <> bsIn))

  mkGraph c MetalPermute' (L _ lTargets :>: L _ lIn :>: ArgsNil) = do
    let bsTargets = getLabelArrDeps lTargets
    let bsIn      = getLabelArrDeps lIn
    wsTargets <- use $ allWriters bsTargets
    wsIn      <- use $ allWriters bsIn
    fusionILP %= wsTargets `allBefore` c
    fusionILP.constraints %= (
      <> inputConstraints c wsIn
      <> [SameFoldSize c])
    fusionILP.bounds %= (<> foldMap (equal (-2) . (`ReadDir` c)) (bsTargets <> bsIn)
                         <> foldMap (equal (-3) . WriteDir c) bsTargets)

  mkGraph c (MetalScan (dirToInt -> dir)) (_fun :>: _exp :>: L _ lIn :>: L _ lOut :>: ArgsNil) = do
    let bsIn  = getLabelArrDeps lIn
    let bsOut = getLabelArrDeps lOut
    wsIn <- use $ allWriters $ getLabelArrDeps lIn
    fusionILP.constraints %= (
      <> inputConstraints c wsIn
      <> [SameFoldSize c])
    fusionILP.bounds %= (<> foldMap (equal dir  . (`ReadDir` c)) bsIn
                         <> foldMap (equal (-3) . WriteDir c) bsOut)
    -- Output size is one larger, so no in-place paths.

  mkGraph c (MetalScan1 (dirToInt -> dir)) (_fun :>: L _ lIn :>: L _ lOut :>: ArgsNil) = do
    let bsIn  = getLabelArrDeps lIn
    let bsOut = getLabelArrDeps lOut
    wsIn <- use $ allWriters bsIn
    fusionILP.constraints %= (
      <> inputConstraints c wsIn
      <> [SameFoldSize c])
    fusionILP.bounds %= (<> foldMap (equal dir . (`ReadDir` c)) bsIn
                         <> foldMap (equal dir . WriteDir c) bsOut)
    fusionILP.inplacePaths %= (<> mkUnitInplacePaths 1 c lIn lOut)

  mkGraph c (MetalScan' (dirToInt -> dir)) (_fun :>: _exp :>: L _ lIn :>: L _ lOut1 :>: L _ lOut2 :>: ArgsNil) = do
    let bsIn   = getLabelArrDeps lIn
    let bsOut1 = getLabelArrDeps lOut1
    let bsOut2 = getLabelArrDeps lOut2
    wsIn <- use $ allWriters $ getLabelArrDeps lIn
    fusionILP.constraints %= (
      <> inputConstraints c wsIn
      <> [NewFoldSize c])
    fusionILP.bounds %= (<> foldMap (equal dir . (`ReadDir` c)) bsIn
                         <> foldMap (equal dir . WriteDir c) (bsOut1 <> bsOut2))
    fusionILP.inplacePaths %= (<> mkUnitInplacePaths 1 c lIn lOut1)

  mkGraph c MetalFold (_fun :>: _exp :>: L _ lIn :>: L _ lOut :>: ArgsNil) = do
    let bsIn  = getLabelArrDeps lIn
    let bsOut = getLabelArrDeps lOut
    wsIn <- use $ allWriters bsIn
    fusionILP.constraints %= (
      <> inputConstraints c wsIn
      <> [NewFoldSize c]
      <> [SameDirection (map (,c) (S.toList bsIn)) (map (c,) (S.toList bsOut))])
    fusionILP.bounds %= (<> defaultBounds bsIn c bsOut)
    -- Not the same shape, so no in-place paths.

  mkGraph c MetalFold1 (_fun :>: L _ lIn :>: L _ lOut :>: ArgsNil) = do
    let bsIn  = getLabelArrDeps lIn
    let bsOut = getLabelArrDeps lOut
    wsIn <- use $ allWriters bsIn
    fusionILP.constraints %= (
      <> inputConstraints c wsIn
      <> [NewFoldSize c]
      <> [SameDirection (map (,c) (S.toList bsIn)) (map (c,) (S.toList bsOut))])
    fusionILP.bounds %= (<> defaultBounds bsIn c bsOut)
    -- Not the same shape, so no in-place paths.

  labelLabelledArg :: Solution -> Node Comp -> LabelledArg env a -> LabelledArgOp MetalOp env a
  labelLabelledArg vars c (L x@(ArgArray In  _ _ _) y) = LOp x y (vars M.! ReadDir  (getLabelArrDep y) c)
  labelLabelledArg vars c (L x@(ArgArray Out _ _ _) y) = LOp x y (vars M.! WriteDir c (getLabelArrDep y))
  labelLabelledArg _ _ (L x y) = LOp x y 0

  getClusterArg :: LabelledArgOp MetalOp env a -> BackendClusterArg MetalOp a
  getClusterArg (LOp _ _ _) = BCAN
  -- For each label: If the output is manifest, then its direction is negative (i.e. not in a backpermuted order)
  finalize g = map NegativeDirIfManifest (S.toList (g^.writeEdges))

  encodeBackendClusterArg (BCAN) = intHost $(hashQ ("BCAN" :: String))

inputConstraints :: Node Comp -> Nodes Comp -> [Constraint]
inputConstraints c = map (`SameFoldSizeIfFused` c) . S.toList

defaultBounds :: Nodes GVal -> Node Comp -> Nodes GVal -> Bounds
defaultBounds bsIn c bsOut = foldMap (lower (-2) . (`ReadDir` c)) bsIn
                          <> foldMap (lower (-2) . WriteDir c) bsOut

instance NFData' (BackendClusterArg MetalOp) where
  rnf' !_ = ()

instance ShrinkArg (BackendClusterArg MetalOp) where
  shrinkArg _ BCAN = BCAN
  deadArg BCAN = BCAN

shrToTypeR :: ShapeR sh -> TypeR sh
shrToTypeR ShapeRz = TupRunit
shrToTypeR (ShapeRsnoc shr) = TupRpair (shrToTypeR shr) (TupRsingle scalarType)
