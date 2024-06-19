module Syn.Expr where

import Syn.Ident
import Syn.Ty
import Parser.Span
import Util ((.:))

import Optics
import GHC.Generics (Generic)

data Expr ann = Expr
  { e :: ExprKind ann,
    ann :: ann,
    span :: Span
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data ExprKind ann
  = ELit Lit
  | EDataCon DataCon
  | EMove Var
  | ERef Ref
  | EDeref (Deref ann)
  | EDerefCall (DerefCall ann)
  | EApp (App ann)
  | ELam (Lam ann)
  | ELet (Let ann)
  | ECase (Case ann)
  | EIf (If ann)
  deriving (Generic, Functor, Foldable, Traversable, Show)

data Lit
  = LBool Bool
  | LInt Integer
  deriving (Generic, Show)

newtype DataCon = DataCon
  { i :: Ident
  }
  deriving (Generic, Eq, Ord, Show)

newtype Var = Var
  { i :: Ident
  }
  deriving (Generic, Eq, Ord, Show)

newtype Ref = Ref
  { v :: Var
  }
  deriving (Generic, Show)

newtype Deref ann = Deref
  { e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

newtype DerefCall ann = DerefCall
  { e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data App ann = App
  { f :: Expr ann,
    x :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data Lam ann = Lam
  { captures :: [Var],
    pat :: Pat,
    e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data Let ann = Let
  { bindings :: [LetBinding ann],
    e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data LetBinding ann
  = BVar (VarBinding ann)
  | BPat (PatBinding ann)
  deriving (Generic, Functor, Foldable, Traversable, Show)

data Case ann = Case
  { e :: Expr ann,
    branches :: [CaseBranch ann]
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data CaseBranch ann = CaseBranch
  { pat :: Pat,
    e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data If ann = If
  { cond :: Expr ann,
    t :: Expr ann,
    e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data VarBinding ann = VarBinding
  { rec :: Bool,
    v :: Var,
    tyAnn :: Maybe QTy,
    e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data PatBinding ann = PatBinding
  { pat :: Pat,
    e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data Pat = Pat
  { pat :: PatKind,
    span :: Span
  }
  deriving (Generic, Show)

data PatKind
  = PLit Lit
  | PVar Var
  | PDecon DeconPat
  deriving (Generic, Show)

data DeconPat = DeconPat
  { underRef :: Bool,
    con :: DataCon,
    pats :: [Pat]
  }
  deriving (Generic, Show)

--------------------------------------------------------------------------------

subExprs :: Traversal' (Expr ann) (Expr ann)
subExprs = #e % exprKindExprs
  where
    exprKindExprs :: Traversal' (ExprKind ann) (Expr ann)
    exprKindExprs = traversalVL $ \f -> \case
      EDeref dr -> EDeref
        <$> traverseOf #e f dr
      EDerefCall drc -> EDerefCall
        <$> traverseOf #e f drc
      EApp (App e1 e2) -> EApp .: App
        <$> f e1
        <*> f e2
      ELam lam -> ELam
        <$> traverseOf #e f lam
      ELet (Let {bindings, e}) -> ELet .: Let
        <$> traverseOf (traversed % letBindingExprs) f bindings
        <*> f e
      ECase (Case {e, branches}) -> ECase .: Case
        <$> f e
        <*> traverseOf (traversed % #e) f branches
      EIf (If {cond, t, e}) -> (EIf <$>) $ If
        <$> f cond
        <*> f t
        <*> f e
      e -> pure e

    letBindingExprs :: Traversal' (LetBinding ann) (Expr ann)
    letBindingExprs = traversalVL $ \f -> \case
      BVar b -> BVar <$> traverseOf #e f b
      BPat b -> BPat <$> traverseOf #e f b

subPats :: Traversal' Pat Pat
subPats = #pat % patKindPats
  where
    patKindPats :: Traversal' PatKind Pat
    patKindPats = traversalVL $ \f -> \case
      PDecon deconPat -> PDecon <$> traverseOf (#pats % traversed) f deconPat
      p -> pure p

patVars :: Pat -> [Var]
patVars = toListOf (cosmosOf subPats % #pat % #_PVar)
