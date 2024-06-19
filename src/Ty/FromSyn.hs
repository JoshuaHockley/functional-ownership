module Ty.FromSyn where

import Ty
import qualified Syn.Ty as Syn

import qualified Data.Set as Set

qTyFromSyn :: Syn.QTy -> QTy
qTyFromSyn Syn.QTy {constraints, ty} = QTy
  { tvs = Set.union (ftv ty') (ftv constraints'),
    constraints = constraints',
    ty = ty'
  }
  where
    ty' = tyFromSyn ty
    constraints' = constraintsFromSyn constraints

tyFromSyn :: Syn.Ty -> Ty
tyFromSyn Syn.Ty {ty} = case ty of
  Syn.TPrim prim -> TPrim prim
  Syn.TCon con -> TCon con
  Syn.TVar tv -> TVar . TVRigid $ RigidTV {name = tv, instanceId = Nothing}
  Syn.TApp Syn.TyApp {t, ts} -> TApp (tyFromSyn t) (fmap tyFromSyn ts)
  Syn.TRef Syn.TyRef {lifetime, ty = ty'} -> TRef (tyFromSyn lifetime) (tyFromSyn ty')
  Syn.TFn Syn.TyFn {a, b, lifetime, onceness} -> TFn
    (tyFromSyn a :->: tyFromSyn b)
    (fnLifetimeFromSyn lifetime)
    (fnOncenessFromSyn onceness)
  Syn.TRegionIntersection Syn.RegionIntersection {rs} ->
    TRegionIntersection $ map tyFromSyn rs
  Syn.TOnceness once ->
    case once of Syn.Once -> TOnce; Syn.Many -> TMany
  where
    fnLifetimeFromSyn = \case
      Syn.FnLifetimeStatic -> LTStatic
      Syn.FnLifetimeAnnotated lt -> tyFromSyn lt
    fnOncenessFromSyn = \case
      Syn.FnOncenessOnce -> TOnce
      Syn.FnOncenessMany -> TMany
      Syn.FnOncenessAnnotated once -> tyFromSyn once

constraintsFromSyn :: Syn.Constraints -> [SimpleConstraint]
constraintsFromSyn = map constraintFromSyn . (.constraints)
  where
    constraintFromSyn :: Syn.Constraint -> SimpleConstraint
    constraintFromSyn Syn.Constraint {constraint} = case constraint of
      Syn.COutlives Syn.OutlivesConstraint {r, s} ->
        COutlives (tyFromSyn r :>: tyFromSyn s)
      Syn.CClass Syn.ClassConstraint {class_, tyArgs} ->
        CClass ClassConstraint {class_, tyArgs = map tyFromSyn tyArgs} 
