module Tc.Solver.Canonical where

import Ty
import Ty.Region
import Ty.Prim
import Tc.Solver.Monad
import Tc.Solver.Types.Canonical
import Tc.Error

import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

canon :: SimpleConstraint -> SolveM [CanonConstraint]
canon = \case
  CTyEq tyEq -> canonTyEq tyEq
  CLiveness liveness -> canonLiveness liveness
  COutlives outlives -> canonOutlives outlives
  CCopyable copyable -> canonCopyable copyable
  CClass _ -> undefined

--------------------------------------------------------------------------------

canonTyEq :: (TyEq Ty Ty) -> SolveM [CanonConstraint]
canonTyEq = canonTyEq' . orient
  where
    canonTyEq' = \case
      t1 :~: t2 | t1 == t2 ->
        pure []
      TVar (TVUnification utv) :~: t
        | Just r <- canonRegion t ->
          pure [CCUTVEq (utv :~: regionFromCanon (filterRegionUTVs utv r))]
        | utv `Set.notMember` fuv t -> pure [CCUTVEq (utv :~: t)]
      TApp t ts :~: TApp t' ts' | length ts == length ts' -> (++)
        <$> canonTyEq (t :~: t')
        <*> (concat <$> traverse canonTyEq (NE.zipWith (:~:) ts ts'))
      TRef lt t :~: TRef lt' t' -> (++)
        <$> canonTyEq (lt :~: lt')
        <*> canonTyEq (t :~: t')
      TFn (a :->: b) lt once :~: TFn (a' :->: b') lt' once' -> concat
        <$> traverse canonTyEq
          [ a :~: a',
            b :~: b',
            lt :~: lt',
            once :~: once'
          ]
      (canonRegion -> Just r) :~: (canonRegion -> Just s)
        | r == s -> pure []
        | otherwise -> pure [CCRegionEq (r :~: s)]
      c -> throwTyError $ EInconsistentTyEq c

orient :: TyEq Ty Ty -> TyEq Ty Ty
orient (t1 :~: t2)
  | tyOrd t1 <= tyOrd t2 = t1 :~: t2
  | otherwise = t2 :~: t1

data TyOrd
  = TyOrdUTV UnificationTV
  | TyOrdOther
  deriving (Eq, Ord)

tyOrd :: Ty -> TyOrd
tyOrd = \case
  TVar (TVUnification utv) -> TyOrdUTV utv
  _ -> TyOrdOther

--------------------------------------------------------------------------------

canonLiveness :: LivenessConstraint Ty -> SolveM [CanonConstraint]
canonLiveness = \case
  (canonRegion -> Just r) :@: p
    | r' `regionIncludes` p,
      all (== p.timeline) (regionTimelines r'),
      null r.utvs
      -> pure []
    | otherwise -> pure [CCLiveness (r :@: p)]
    where
      r' = cutRegionBelowEq p.timeline.depth r.concrete
  c -> throwTyError $ EInconsistentLivenessConstraint c

--------------------------------------------------------------------------------

canonOutlives :: OutlivesConstraint Ty -> SolveM [CanonConstraint]
canonOutlives = \case
  (canonRegion -> Just r) :>: (canonRegion -> Just s)
    | r.concrete == totalRegion, null r.rtvs, null r.utvs
      -> pure []
    | r.concrete `regionOutlives` s.concrete,
      r.rtvs `Set.isSubsetOf` s.rtvs,
      all null [r.utvs, s.utvs]
      -> pure []
    | otherwise -> pure [CCOutlives (r :>: s)]
  c -> throwTyError $ EInconsistentOutlivesConstraint c

--------------------------------------------------------------------------------

canonCopyable :: CopyableConstraint Ty -> SolveM [CanonConstraint]
canonCopyable (CopyableConstraint t) = case t of
  TPrim p -> case p of
    PrimBool -> pure []
    PrimInt -> pure []
  TRef {} -> pure []
  TVar (TVUnification utv) -> pure [CCCopyable (CopyableConstraint utv)]
  _ -> throwTyError $ EUncopyable t

--------------------------------------------------------------------------------

canonRegion :: Ty -> Maybe CanonRegion
canonRegion = \case
  TVar tv -> Just $ case tv of
    TVRigid rtv -> rtvCanonRegion rtv
    TVUnification utv -> utvCanonRegion (utv, Nothing)
  TRegion r -> Just $ concreteCanonRegion r
  TRegionCut {above, r} -> fmap (cutCanonRegion above) $ canonRegion r
  TRegionIntersection rs -> mconcat <$> traverse canonRegion rs
  _ -> Nothing
