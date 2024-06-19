{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ty.Pretty where

import Ty
import Ty.Region
import Pretty
import Syn.Pretty ()

import Prettyprinter
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

instance Pretty QTy where
  pretty QTy {tvs, constraints, ty}
    = quantifier <> qualifier <> pretty ty
    where
      quantifier
        | Set.null tvs = emptyDoc
        | otherwise = prettyLit "forall" <+> hsep tvs' <+> prettyLit "." <> space
        where
          tvs' = map pretty $ Set.toList tvs

      qualifier
        | null constraints = emptyDoc
        | otherwise = parens (hsep (punctuate comma constraints'))
          <+> prettyLit "=>" <> space
        where
          constraints' = map pretty constraints

instance Pretty Ty where
  pretty = \case
    TPrim prim -> pretty prim
    TCon c -> pretty c
    TVar tv -> pretty tv
    TApp t ts -> parens (pretty t <+> hsep (map pretty (NE.toList ts)))
    TRef lt ty ->
      prettyLit "&" <> pretty lt <+> pretty ty
    TFn (t1 :->: t2) lt once ->
      parens (pretty t1
        <+> prettyLit "->" <> braces (pretty once) <> pretty lt
        <+> pretty t2)
    TRegion r -> pretty r
    TRegionCut above r -> pretty r <> prettyLit "/" <> pretty above
    TRegionIntersection rs -> brackets . hsep . punctuate comma $ map pretty rs
    TOnceness once -> pretty once

instance Pretty TyVar where
  pretty = \case
    TVRigid rtv -> pretty rtv
    TVUnification utv -> pretty utv

instance Pretty RigidTV where
  pretty RigidTV {name, instanceId}
    = squote <> pretty name <> instanceTag
    where
      instanceTag = maybe emptyDoc pretty instanceId

instance Pretty UnificationTV where
  pretty UnificationTV {id}
    = squote <> pretty id

deriving newtype instance Pretty TyId

instance Pretty Region where
  pretty Region {endPoints}
    = brackets . hsep . punctuate comma . map pretty $ Set.toList endPoints

instance Pretty Point where
  pretty p
    = dot <> pretty p.pointKey.key <> parens (pretty p.timeline)

instance Pretty Timeline where
  pretty Timeline {id, depth}
    = pretty id <> slash <> pretty depth

deriving newtype instance Pretty TimelineDepth

deriving newtype instance Pretty TimelineId

instance Pretty Onceness where
  pretty = prettyLit . \case
    Once -> "Once"
    Many -> "Many"

instance Pretty SimpleConstraint where
  pretty = \case
    CTyEq tyEq -> pretty tyEq
    CLiveness liveness -> pretty liveness
    COutlives outlives -> pretty outlives
    CCopyable copyable -> pretty copyable
    CClass classConstraint -> undefined

instance (Pretty t1, Pretty t2) => Pretty (TyEq t1 t2) where
  pretty (t1 :~: t2) = pretty t1 <+> prettyLit "~" <+> pretty t2

instance Pretty ty => Pretty (LivenessConstraint ty) where
  pretty (r :@: p) = pretty r <+> prettyLit "@" <+> pretty p

instance Pretty ty => Pretty (OutlivesConstraint ty) where
  pretty (r :>: s) = pretty r <+> prettyLit ">" <+> pretty s

instance Pretty ty => Pretty (CopyableConstraint ty) where
  pretty (CopyableConstraint t) = prettyLit "copyable" <> parens (pretty t)
