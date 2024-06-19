{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tc.Error.Pretty where

import Pretty
import Tc.Error
import Tc.Gen.Liveness
import Syn.Pretty ()
import Ty.Pretty ()

import Prettyprinter

instance Pretty TyError where
  pretty = \case
    EVarNotInScope v ->
      prettyLit "Variable not in scope:" <+> pretty v
    EDataConNotInScope c ->
      prettyLit "Data constructor not in scope:" <+> pretty c
    EInvalidDataConTy t ->
      prettyLit "Invalid data constructor type:" <+> pretty t
    EPatDeconMissmatchedArity c ->
      prettyLit "Missmatched arity of data constructor in pattern:" <+> pretty c
    EInvalidRecursiveDefinition ->
      prettyLit "Invalid recursive definition"

    ETopLevelMove v ->
      prettyLit "Top level move:" <+> pretty v
    EDoubleMove DoubleMove {v} ->
      prettyLit "Double move:" <+> pretty v
    EOutOfTimelineMove OutOfTimelineMove {v} ->
      prettyLit "Move out of timeline:" <+> pretty v

    EInconsistentTyEq tyEq ->
      prettyLit "Inconsistent type equality:" <+> pretty tyEq
    EInconsistentLivenessConstraint livenessConstraint ->
      prettyLit "Inconsistent liveness constraint:" <+> pretty livenessConstraint
    EInconsistentOutlivesConstraint outlivesConstraint ->
      prettyLit "Inconsistent outlives constraint:" <+> pretty outlivesConstraint
    EUncopyable t ->
      prettyLit "Tried to copy an uncopyable type:" <+> pretty t

    EUnsolvedConstraintsTopLevel cs ->
      prettyLit "Unsolved constraints:" <+> sep (punctuate comma (map pretty cs))
    EUnsolvedConstraintsUnderImplication cs ->
      prettyLit "Unsolved constraints:" <+> sep (punctuate comma (map pretty cs))
