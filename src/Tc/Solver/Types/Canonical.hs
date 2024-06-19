module Tc.Solver.Types.Canonical where

import Ty
import Ty.Region

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Optics
import GHC.Generics (Generic)

data CanonConstraint
  = CCUTVEq (TyEq UnificationTV Ty)
  | CCRegionEq (TyEq CanonRegion CanonRegion)
  | CCLiveness (LivenessConstraint CanonRegion)
  | CCOutlives (OutlivesConstraint CanonRegion)
  | CCCopyable (CopyableConstraint UnificationTV)
  deriving (Generic, Show)

--------------------------------------------------------------------------------

data CanonRegion = CanonRegion
  { concrete :: Region,
    rtvs :: Set RigidTV,
    utvs :: Map UnificationTV (Maybe TimelineDepth)
  }
  deriving (Generic, Eq, Show)

instance Monoid CanonRegion where
  mempty = CanonRegion
    { concrete = totalRegion,
      rtvs = Set.empty,
      utvs = M.empty
    }

instance Semigroup CanonRegion where
  r <> s = CanonRegion
    { concrete = r.concrete <> s.concrete,
      rtvs = Set.union r.rtvs s.rtvs,
      utvs = M.unionWith (liftA2 max) r.utvs s.utvs
    }

concreteCanonRegion :: Region -> CanonRegion
concreteCanonRegion = (mempty &) . (#concrete .~)

rtvCanonRegion :: RigidTV -> CanonRegion
rtvCanonRegion = (mempty &) . (#rtvs .~) . Set.singleton

utvCanonRegion :: (UnificationTV, Maybe TimelineDepth) -> CanonRegion
utvCanonRegion = (mempty &) . (#utvs .~) . uncurry M.singleton

cutCanonRegion :: TimelineDepth -> CanonRegion -> CanonRegion
cutCanonRegion d CanonRegion {concrete, utvs, rtvs}
  = CanonRegion
    { concrete = cutRegionAbove d concrete,
      utvs = fmap (Just . maybe d (min d)) utvs,
      rtvs
    }

filterRegionUTVs :: UnificationTV -> CanonRegion -> CanonRegion
filterRegionUTVs utv = #utvs %~ (M.filterWithKey (\utv' _ -> utv' /= utv))

--------------------------------------------------------------------------------

fromCanon :: CanonConstraint -> SimpleConstraint
fromCanon = \case
  CCUTVEq (utv :~: t) ->
    CTyEq (TVar (TVUnification utv) :~: t)
  CCRegionEq (r :~: s) ->
    CTyEq (regionFromCanon r :~: regionFromCanon s)
  CCLiveness (r :@: p) ->
    CLiveness (regionFromCanon r :@: p)
  CCOutlives (r :>: s) ->
    COutlives (regionFromCanon r :>: regionFromCanon s)
  CCCopyable (CopyableConstraint utv) ->
    CCopyable . CopyableConstraint . TVar $ TVUnification utv

regionFromCanon :: CanonRegion -> Ty
regionFromCanon = unwrapSingleton . regionFromCanon'
  where
    regionFromCanon' CanonRegion {concrete, utvs, rtvs}
      = TRegionIntersection $
        (if concrete /= totalRegion then [TRegion concrete] else [])
        ++ map (TVar . TVRigid) (Set.toList rtvs)
        ++ map regionUTVFromCanon (M.toList utvs)
    regionUTVFromCanon (utv, mCutAbove)
     = (maybe id TRegionCut mCutAbove) (TVar (TVUnification utv))
    unwrapSingleton = \case
      TRegionIntersection [r] -> r
      r -> r
