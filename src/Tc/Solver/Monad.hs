module Tc.Solver.Monad where

import Tc.Solver.Types
import Tc.Solver.Types.Canonical
import Tc.Error

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Optics
import Optics.State.Operators
import GHC.Generics (Generic)

newtype SolveM a = SolveM
  { solve :: StateT SolverState (Except TyError) a
  }
  deriving Generic
  deriving newtype (Functor, Applicative, Monad)

runSolveM :: SolveM a -> Except TyError a
runSolveM SolveM {solve} = evalStateT solve initialSolverState

throwTyError :: TyError -> SolveM a
throwTyError = SolveM . throwError

--------------------------------------------------------------------------------

data SolverState = SolverState
  { workSet :: WorkSet,
    inertSet :: InertSet
  }
  deriving (Generic, Show)

initialSolverState :: SolverState
initialSolverState = SolverState {workSet = WorkSet [], inertSet = InertSet []}

pushConstraint :: GivenOrWanted CanonConstraint -> SolveM ()
pushConstraint c = SolveM $ #workSet % #cs %= (c :)

getConstraint :: SolveM (Maybe (GivenOrWanted CanonConstraint))
getConstraint = SolveM $ do
  WorkSet cs <- use #workSet
  case cs of
    c : cs' -> assign #workSet (WorkSet cs') *> pure (Just c)
    [] -> pure Nothing

pushInertConstraint :: GivenOrWanted CanonConstraint -> SolveM ()
pushInertConstraint c = SolveM $ #inertSet % #cs %= (c :)

getInerts :: SolveM InertSet
getInerts = SolveM $ use #inertSet

filterInertsM :: (GivenOrWanted CanonConstraint -> SolveM Bool) -> SolveM ()
filterInertsM f = do
  inerts <- getInerts
  inerts' <- traverseOf #cs (filterM f) inerts
  SolveM $ #inertSet .= inerts'
