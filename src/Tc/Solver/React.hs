module Tc.Solver.React where

import Ty
import Ty.Region
import Tc.Solver.Types
import Tc.Solver.Types.Canonical

import qualified Data.Set as Set
import Control.Applicative
import Optics

class (ConstraintMode aux, ConstraintMode target) => React aux target where
  react
    :: aux CanonConstraint
    -> target CanonConstraint
    -> ReactResult (target SimpleConstraint)

data ReactResult constraint
  = RProduct constraint
  | RDischarge
  | RNoReaction
  deriving (Functor)

instance React GivenOrWanted GivenOrWanted where
  react (CGiven aux) (CGiven target) = CGiven <$> react aux target
  react (CGiven aux) (CWanted target) = CWanted <$> react aux target
  react (CWanted _) (CGiven _) = RNoReaction
  react (CWanted aux) (CWanted target) = CWanted <$> react aux target

instance React Given Given where
  react (Given aux) (Given target) = Given <$> reactRaw aux target

instance React Given Wanted where
  react (Given aux) (Wanted target) = Wanted <$> reactRaw aux target

instance React Wanted Wanted where
  react (Wanted aux) (Wanted target) = Wanted <$> reactRaw aux target

--------------------------------------------------------------------------------

reactRaw :: CanonConstraint -> CanonConstraint -> ReactResult SimpleConstraint
reactRaw (CCUTVEq utvEq) c
  = reactWithAuxUTVEq utvEq c
reactRaw (CCOutlives outlives) (CCOutlives outlives')
  = COutlives <$> reactOutlives outlives outlives'
reactRaw _ _ = RNoReaction

--------------------------------------------------------------------------------

reactWithAuxUTVEq
  :: TyEq UnificationTV Ty
  -> CanonConstraint
  -> ReactResult SimpleConstraint
reactWithAuxUTVEq utvEq = \case
  CCUTVEq utvEq' -> CTyEq <$> reactUTVEqs utvEq utvEq'
  CCRegionEq rEq -> CTyEq <$> subRegionEqWith utvEq rEq
  CCLiveness liveness -> CLiveness <$> subLivenessWith utvEq liveness
  CCOutlives outlives -> COutlives <$> subOutlivesWith utvEq outlives
  CCCopyable copyable -> CCopyable <$> subCopyableWith utvEq copyable

reactUTVEqs
  :: TyEq UnificationTV Ty
  -> TyEq UnificationTV Ty
  -> ReactResult (TyEq Ty Ty)
reactUTVEqs (utv1 :~: t1) (utv2 :~: t2)
  | utv1 == utv2 = RProduct $ t1 :~: t2
  | Just t2' <- subWithUTVEq (utv1 :~: t1) t2
    = RProduct $ TVar (TVUnification utv2) :~: t2'
  | otherwise = RNoReaction

subRegionEqWith
  :: TyEq UnificationTV Ty
  -> TyEq CanonRegion CanonRegion
  -> ReactResult (TyEq Ty Ty)
subRegionEqWith utvEq (r :~: s)
  | Just r'' <- subWithUTVEq utvEq r'
    = RProduct $ r'' :~: s'
  | Just s'' <- subWithUTVEq utvEq s'
    = RProduct $ r' :~: s''
  | otherwise = RNoReaction
  where (r', s') = (regionFromCanon r, regionFromCanon s)

subLivenessWith
  :: TyEq UnificationTV Ty
  -> LivenessConstraint CanonRegion
  -> ReactResult (LivenessConstraint Ty)
subLivenessWith utvEq (r :@: p)
  | Just r'' <- subWithUTVEq utvEq r'
    = RProduct $ r'' :@: p
  | otherwise = RNoReaction
  where r' = regionFromCanon r

subOutlivesWith
  :: TyEq UnificationTV Ty
  -> OutlivesConstraint CanonRegion
  -> ReactResult (OutlivesConstraint Ty)
subOutlivesWith utvEq (r :>: s)
  | Just r'' <- subWithUTVEq utvEq r'
    = RProduct $ r'' :>: s'
  | Just s'' <- subWithUTVEq utvEq s'
    = RProduct $ r' :>: s''
  | otherwise = RNoReaction
  where (r', s') = (regionFromCanon r, regionFromCanon s)

subCopyableWith
  :: TyEq UnificationTV Ty
  -> CopyableConstraint UnificationTV
  -> ReactResult (CopyableConstraint Ty)
subCopyableWith utvEq (CopyableConstraint utv)
  = maybe RNoReaction (RProduct . CopyableConstraint) $
      subWithUTVEq utvEq (TVar (TVUnification utv))

subWithUTVEq :: TyEq UnificationTV Ty -> Ty -> Maybe Ty
subWithUTVEq (utv :~: t1) t2
  | utv `Set.member` fuv t2
    = Just $ applySingularTySub (TVUnification utv, t1) t2
  | otherwise = Nothing

--------------------------------------------------------------------------------

reactOutlives
  :: OutlivesConstraint CanonRegion
  -> OutlivesConstraint CanonRegion
  -> ReactResult (OutlivesConstraint Ty)
reactOutlives (r1 :>: s1) (r2 :>: s2)
  | r1.rtvs `Set.isSubsetOf` r2.rtvs,
    s1.rtvs `Set.isSubsetOf` s2.rtvs,
    all (\r -> r.concrete == totalRegion && null r.utvs) [r1, s1],
    not (null r2.rtvs)
    = RProduct (regionFromCanon r2' :>: regionFromCanon s2)
  | otherwise = RNoReaction
  where
    r2' = r2 & #rtvs %~ (Set.\\ r1.rtvs)

--------------------------------------------------------------------------------

instance Alternative ReactResult where
  empty = RNoReaction
  r1 <|> r2 = case r1 of
    RProduct _ -> r1
    _ -> r2

instance Applicative ReactResult where
  pure = RProduct
  r1 <*> r2 = case r1 of
    RProduct f -> fmap f r2
    RDischarge -> RDischarge
    RNoReaction -> RNoReaction
