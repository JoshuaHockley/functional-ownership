module Tc.Gen where

import Ty
import Tc.Gen.Monad
import Tc.Gen.Decl
import Tc.Gen.Liveness
import Tc.Error
import Syn.Decl

import Control.Monad.Except
import Optics

genConstraints :: Program () -> Except TyError (Program Ty, Constraints)
genConstraints prog = do
  (prog', out) <- runGenM $ genProgram prog
  livenessConstraints <- withExcept (livenessTyError #) $
    consolidateLiveness out.liveness
  let constraints' = out.constraints <> livenessConstraints
  pure (prog', constraints')
