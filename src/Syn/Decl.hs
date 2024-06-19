module Syn.Decl where

import Syn.Expr
import Syn.Ty
import Parser.Span

import Optics
import GHC.Generics (Generic)

data Program ann = Program
  { decls :: [Decl ann],
    e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data Decl ann = Decl
  { d :: DeclKind ann,
    span :: Span
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data DeclKind ann
  = DBinding (VarBinding ann)
  | DData DataDecl
  | DClass ClassDecl
  | DInstance (InstanceDecl ann)
  deriving (Generic, Functor, Foldable, Traversable, Show)

data DataDecl = DataDecl
  { tyCon :: TyCon,
    tyVars :: [TyVar],
    conSigs :: [DataConSig]
  }
  deriving (Generic, Show)

data DataConSig = DataConSig
  { con :: DataCon,
    ty :: QTy
  }
  deriving (Generic, Show)

data ClassDecl = ClassDecl
  { class_ :: Class,
    tyVars :: [TyVar],
    memberSigs :: [MemberSig]
  }
  deriving (Generic, Show)

data MemberSig = MemberSig
  { member :: Var,
    ty :: QTy
  }
  deriving (Generic, Show)

data InstanceDecl ann = InstanceDecl
  { constraints :: Constraints,
    instance_ :: ClassConstraint,
    memberImpls :: [MemberImpl ann]
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data MemberImpl ann = MemberImpl
  { member :: Var,
    e :: Expr ann
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

--------------------------------------------------------------------------------

programExprs :: Traversal' (Program ann) (Expr ann)
programExprs = traversalVL $ \f Program {decls, e} -> Program
  <$> traverseOf (traversed % #d % declKindExprs) f decls
  <*> f e
  where
    declKindExprs :: Traversal' (DeclKind ann) (Expr ann)
    declKindExprs = traversalVL $ \f -> \case
      DBinding b -> DBinding
        <$> traverseOf #e f b
      DInstance instanceDecl -> DInstance
        <$> traverseOf (#memberImpls % traversed % #e) f instanceDecl
      d -> pure d
