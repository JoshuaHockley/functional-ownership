module Tc where

import Ty
import Tc.Gen
import Tc.Solver
import Tc.Error
import Syn.Decl

import Control.Monad
import Control.Monad.Except

typeCheckProgram :: Program () -> Except (TyError, Maybe (Program Ty)) (Program Ty)
typeCheckProgram prog = do
  (prog', constraints) <- withExcept (, Nothing) $ genConstraints prog
  (unifier, residual) <- withExcept (, Just prog') $ solveConstraints constraints
  when (not (null residual)) $
    throwError (EUnsolvedConstraintsTopLevel residual, Just prog')
  pure $ applyUnifier unifier prog'
