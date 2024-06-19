{-# OPTIONS_GHC -fno-warn-orphans #-}

module Syn.Pretty where

import Syn.Decl
import Syn.Expr
import Syn.Ty
import Syn.Ident
import Ty.Prim
import Pretty

import qualified Data.List.NonEmpty as NE
import Prettyprinter
import Optics

instance Pretty (Program ann) where
  pretty Program {decls, e}
    = vsep [decls', semi <> semi, e']
    where
      decls' = vsep . punctuate semi $ map pretty decls
      e' = pretty e

instance Pretty (Decl ann) where
  pretty Decl {d} = case d of
    DBinding b -> pretty b
    DData dataDecl -> pretty dataDecl
    DClass classDecl -> undefined
    DInstance instanceDecl -> undefined

instance Pretty DataDecl where
  pretty DataDecl {tyCon, tyVars, conSigs}
    = top <+> braces (nest' (line <> vsep conSigs') <> line)
    where
      top = prettyLit "data" <+> pretty tyCon <+> hsep (map pretty tyVars)
      conSigs' = punctuate semi $ map pretty conSigs

instance Pretty DataConSig where
  pretty DataConSig {con, ty}
    = pretty con <+> colon <+> pretty ty

--------------------------------------------------------------------------------

instance Pretty (Expr ann) where
  pretty Expr {e} = case e of
    ELit l -> pretty l
    EDataCon c -> pretty c
    EMove v -> pretty v
    ERef r -> pretty r
    EDeref dr -> pretty dr
    EDerefCall drc -> pretty drc
    EApp app -> pretty app
    ELam lam -> pretty lam
    ELet let_ -> pretty let_
    ECase case_ -> pretty case_
    EIf if_ -> pretty if_

instance Pretty Lit where
  pretty = \case
    LBool b -> case b of
      False -> prettyLit "false"
      True -> prettyLit "true"
    LInt n -> pretty n

deriving newtype instance Pretty DataCon

deriving newtype instance Pretty Var

instance Pretty Ref where
  pretty Ref {v}
    = prettyLit "&" <> pretty v

instance Pretty (Deref ann) where
  pretty Deref {e}
    = prettyLit "*" <> parens (pretty e)

instance Pretty (DerefCall ann) where
  pretty DerefCall {e}
    = prettyLit "~" <> parens (pretty e)

instance Pretty (App ann) where
  pretty App {f, x}
    = parens $ sep [pretty f, pretty x]

instance Pretty (Lam ann) where
  pretty Lam {captures, pat, e}
    = parens $ backslash <> captures' <> pretty pat
    <+> prettyLit "->" <> nest' (line <> pretty e)
    where
      captures'
        | null captures = emptyDoc
        | otherwise = brackets (hsep (map pretty captures)) <> space

instance Pretty (Let ann) where
  pretty Let {bindings, e}
    = prettyLit "let" <+> align (vsep bindings') <> line
    <> prettyLit "in" <+> align e'
    where
      bindings' = map pretty bindings
      e' = pretty e

instance Pretty (LetBinding ann) where
  pretty = \case
    BVar b -> pretty b
    BPat b -> pretty b

instance Pretty (Case ann) where
  pretty Case {e, branches}
    = prettyLit "case" <+> e'
    <+> prettyLit "of" <> nest' (line <> vsep branches')
    where
      e' = pretty e
      branches' = punctuate semi $ map pretty branches

instance Pretty (CaseBranch ann) where
  pretty CaseBranch {pat, e}
    = pretty pat <+> prettyLit "->" <+> nest' (pretty e)

instance Pretty (If ann) where
  pretty If {cond, t, e}
    = parens (prettyLit "if" <+> pretty cond <> nest'
        (line <> vsep
          [ prettyLit "then" <+> pretty t,
            prettyLit "else" <+> pretty e
          ]
        ))

instance Pretty (VarBinding ann) where
  pretty VarBinding {rec, v, tyAnn, e}
    = rec' <> pretty v <+> tyAnn' <> nest' (line <> equals <+> pretty e)
    where
      rec'
        | rec = prettyLit "rec" <> space
        | otherwise = emptyDoc
      tyAnn' = case tyAnn of
        Just ty -> prettyLit ":" <+> pretty ty <> space
        _ -> emptyDoc

instance Pretty (PatBinding ann) where
  pretty PatBinding {pat, e}
    = pretty pat <+> prettyLit "=" <+> nest' (pretty e)

instance Pretty Pat where
  pretty Pat {pat} = case pat of
    PLit l -> pretty l
    PVar v -> pretty v
    PDecon decon -> pretty decon

instance Pretty DeconPat where
  pretty DeconPat {underRef, con, pats}
    = parens (ref <> con' <> pats')
    where
      ref
        | underRef = prettyLit "&"
        | otherwise = emptyDoc
      con' = pretty con
      pats'
        | null pats = emptyDoc
        | otherwise = space <> hsep (map pretty pats)


--------------------------------------------------------------------------------

instance Pretty QTy where
  pretty QTy {constraints, ty}
    = context <> nest' (pretty ty)
    where
      context
        | null constraints.constraints = emptyDoc
        | otherwise = pretty constraints <+> prettyLit "=>" <> space

instance Pretty Ty where
  pretty Ty {ty} = case ty of
    TPrim prim -> pretty prim
    TCon c -> pretty c
    TVar v -> pretty v
    TApp app -> pretty app
    TRef ref -> pretty ref
    TFn fn -> pretty fn
    TRegionIntersection rs -> pretty rs
    TOnceness once -> pretty once

instance Pretty PrimTy where
  pretty = prettyLit . \case
    PrimBool -> "Bool"
    PrimInt -> "Int"

deriving newtype instance Pretty TyCon

deriving newtype instance Pretty TyVar

instance Pretty TyApp where
  pretty TyApp {t, ts}
    = parens (pretty t <+> hsep (map pretty (NE.toList ts)))

instance Pretty TyRef where
  pretty TyRef {lifetime, ty}
    = prettyLit "&" <> prettyLifetimeBracketed lifetime <+> pretty ty

instance Pretty TyFn where
  pretty TyFn {a, b, lifetime, onceness}
    = parens (pretty a <+> prettyLit "->" <> onceness' <> lifetime' <+> pretty b)
    where
      onceness' = case onceness of
        FnOncenessOnce -> prettyLit "*"
        FnOncenessMany -> emptyDoc
        FnOncenessAnnotated once -> braces (pretty once)
      lifetime' = case lifetime of
        FnLifetimeStatic -> emptyDoc
        FnLifetimeAnnotated lt -> prettyLifetimeBracketed lt <> space

instance Pretty RegionIntersection where
  pretty RegionIntersection {rs}
    = brackets . hsep . punctuate comma $ map pretty rs

instance Pretty Onceness where
  pretty = prettyLit . \case
    Once -> "Once"
    Many -> "Many"

instance Pretty Constraints where
  pretty Constraints {constraints}
    = parens . hsep . punctuate comma $ map pretty constraints

instance Pretty Constraint where
  pretty Constraint {constraint} = case constraint of
    COutlives outlivesConstraint -> pretty outlivesConstraint
    CClass classConstraint -> undefined

instance Pretty OutlivesConstraint where
  pretty OutlivesConstraint {r, s}
    = pretty r <+> prettyLit ">" <+> pretty s

prettyLifetimeBracketed :: Ty -> Doc ann
prettyLifetimeBracketed lt
  | has (#ty % #_TRegionIntersection) lt = pretty lt
  | otherwise = brackets (pretty lt)

--------------------------------------------------------------------------------

deriving newtype instance Pretty Ident
