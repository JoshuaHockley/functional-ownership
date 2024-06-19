module Tc.Solver.Types where

import Ty
import Tc.Solver.Types.Canonical

import Data.Map (Map)
import Optics
import Data.Kind
import GHC.Generics (Generic)

newtype Given constraint = Given
  { c :: constraint
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

newtype Wanted constraint = Wanted
  { c :: constraint
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

newtype Residual constraint = Residual
  { c :: constraint
  }
  deriving (Generic, Functor, Foldable, Traversable, Show)

data GivenOrWanted constraint
  = CGiven (Given constraint)
  | CWanted (Wanted constraint)
  deriving (Generic, Functor, Foldable, Traversable, Show)

class ConstraintMode (mode :: Type -> Type)
instance ConstraintMode Given
instance ConstraintMode Wanted
instance ConstraintMode Residual
instance ConstraintMode GivenOrWanted

distributeMode
  :: (ConstraintMode mode, Traversable mode)
  => mode [constraint] -> [mode constraint]
distributeMode = sequenceA

--------------------------------------------------------------------------------

newtype Unifier = Unifier
  { u :: Map UnificationTV Ty
  }

--------------------------------------------------------------------------------

newtype WorkSet = WorkSet
  { cs :: [GivenOrWanted CanonConstraint]
  }
  deriving (Generic, Show)

newtype InertSet = InertSet
  { cs :: [GivenOrWanted CanonConstraint]
  }
  deriving (Generic, Show)

inertWanteds :: Traversal' InertSet (Wanted CanonConstraint)
inertWanteds = #cs % traversed % #_CWanted
