module Tc.Error where

import Syn.Expr
import Ty
import Tc.Gen.Liveness

import Optics

data TyError
  = EVarNotInScope Var
  | EDataConNotInScope DataCon
  | EInvalidDataConTy QTy
  | EPatDeconMissmatchedArity DataCon
  | EInvalidRecursiveDefinition

  | ETopLevelMove Var
  | EDoubleMove DoubleMove
  | EOutOfTimelineMove OutOfTimelineMove

  | EInconsistentTyEq (TyEq Ty Ty)
  | EInconsistentLivenessConstraint (LivenessConstraint Ty)
  | EInconsistentOutlivesConstraint (OutlivesConstraint Ty)
  | EUncopyable Ty

  | EUnsolvedConstraintsTopLevel [SimpleConstraint]
  | EUnsolvedConstraintsUnderImplication [SimpleConstraint]
  deriving Show

--------------------------------------------------------------------------------

livenessTyError :: Prism' TyError LivenessError
livenessTyError = prism' asTyError toLivenessError
  where
    asTyError = \case
      LEDoubleMove m -> EDoubleMove m
      LEOutOfTimelineMove m -> EOutOfTimelineMove m
    toLivenessError = \case
      EDoubleMove m -> Just $ LEDoubleMove m
      EOutOfTimelineMove m -> Just $ LEOutOfTimelineMove m
      _ -> Nothing
