module Syn.Ty where

import Ty.Prim
import Syn.Ident
import Parser.Span

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

data QTy = QTy
  { constraints :: Constraints,
    ty :: Ty,
    span :: Span
  }
  deriving (Generic, Show)

data Ty = Ty
  { ty :: TyKind,
    span :: Span
  }
  deriving (Generic, Show)

data TyKind
  = TPrim PrimTy
  | TCon TyCon
  | TVar TyVar
  | TApp TyApp
  | TRef TyRef
  | TFn TyFn
  | TRegionIntersection RegionIntersection
  | TOnceness Onceness
  deriving (Generic, Show)

newtype TyCon = TyCon
  { i :: Ident
  }
  deriving (Generic, Eq, Show)

newtype TyVar = TyVar
  { i :: Ident
  }
  deriving (Generic, Eq, Ord, Show)

data TyApp = TyApp
  { t :: Ty,
    ts :: NonEmpty Ty
  }
  deriving (Generic, Show)

data TyRef = TyRef
  { lifetime :: Ty,
    ty :: Ty
  }
  deriving (Generic, Show)

data TyFn = TyFn
  { a :: Ty,
    b :: Ty,
    lifetime :: FnLifetime,
    onceness :: FnOnceness
  }
  deriving (Generic, Show)

data FnLifetime
  = FnLifetimeStatic
  | FnLifetimeAnnotated Ty
  deriving (Generic, Show)

data FnOnceness
  = FnOncenessOnce
  | FnOncenessMany
  | FnOncenessAnnotated Ty
  deriving (Generic, Show)

newtype RegionIntersection = RegionIntersection
  { rs :: [Ty]
  }
  deriving (Generic, Show)

data Onceness = Once | Many
  deriving (Generic, Show)

--------------------------------------------------------------------------------

newtype Constraints = Constraints
  { constraints :: [Constraint]
  }
  deriving (Generic, Show)

data Constraint = Constraint
  { constraint :: ConstraintKind,
    span :: Span
  }
  deriving (Generic, Show)

data ConstraintKind
  = COutlives OutlivesConstraint
  | CClass ClassConstraint
  deriving (Generic, Show)

data OutlivesConstraint = OutlivesConstraint
  { r :: Ty,
    s :: Ty
  }
  deriving (Generic, Show)

data ClassConstraint = ClassConstraint
  { class_ :: Class,
    tyArgs :: [Ty]
  }
  deriving (Generic, Show)

newtype Class = Class
  { i :: Ident
  }
  deriving (Generic, Show)
