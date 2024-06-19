module Tc.Solver where

import Ty
import Tc.Solver.Monad
import Tc.Solver.Types
import Tc.Solver.Types.Canonical
import Tc.Solver.Canonical
import Tc.Solver.React
import Tc.Error

import qualified Data.Map as M
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Except
import Optics

solveConstraints :: Constraints -> Except TyError (Unifier, [SimpleConstraint])
solveConstraints wanted = do
  (u, Residual r) <- solveConstraints' (Given []) (Wanted wanted)
  pure (u, r)

solveConstraints'
  :: Given [SimpleConstraint]
  -> Wanted Constraints
  -> Except TyError (Unifier, Residual [SimpleConstraint])
solveConstraints' (Given given) (Wanted wanted) = do
  (unifier, Residual residual) <- solveSimples (Given given) (Wanted wanted.simples)
  for_ (applyUnifier unifier wanted.implics) $ \ImplicConstraint {antecedent, consequent} -> do
    let given' = given <> residual <> antecedent
    (_, Residual residual') <- solveConstraints' (Given given') (Wanted consequent)
    when (not (null residual')) $
      throwError $ EUnsolvedConstraintsUnderImplication residual'
  pure (unifier, Residual residual)

solveSimples
  :: Given [SimpleConstraint]
  -> Wanted [SimpleConstraint]
  -> Except TyError (Unifier, Residual [SimpleConstraint])
solveSimples given wanted = do
  inerts <- runSolveM $ solve given wanted
  pure $ constructUnifier inerts
  where
    constructUnifier :: InertSet -> (Unifier, Residual [SimpleConstraint])
    constructUnifier inerts = (unifier, residual')
      where
        (residual, utvEqs) = partitionEithers $
          map (matching #_CCUTVEq) (inerts ^.. inertWanteds % #c)
        unifier = Unifier . M.fromList $
          [(utv, applyUnifier unifier t) | utv :~: t <- utvEqs]
        residual' = Residual $ map (applyUnifier unifier . fromCanon) residual

applyUnifier :: HasTys a => Unifier -> a -> a
applyUnifier Unifier {u}
  = (tys %~ transformOf subTys simplify)
  . applyTySub (M.mapKeys TVUnification u)
  where
    simplify t = fromMaybe t (simplifyRegion t)
      where
        simplifyRegion = fmap regionFromCanon . canonRegion

--------------------------------------------------------------------------------

solve :: Given [SimpleConstraint] -> Wanted [SimpleConstraint] -> SolveM InertSet
solve given wanted = do
  initWorkList given wanted
  loop
  where
    loop = getConstraint >>= \case
      Just c -> processConstraint c >> loop
      Nothing -> getInerts

initWorkList :: Given [SimpleConstraint] -> Wanted [SimpleConstraint] -> SolveM ()
initWorkList given wanted =
  traverse_ pushConstraint' $
    map CGiven (distributeMode given) ++ map CWanted (distributeMode wanted)

processConstraint :: GivenOrWanted CanonConstraint -> SolveM ()
processConstraint c
  = reactAsTarget >>= \case
      RProduct c' -> pushConstraint' c'
      RDischarge -> pure ()
      RNoReaction -> do
        reactAsAux
        pushInertConstraint c
  where
    reactAsTarget :: SolveM (ReactResult (GivenOrWanted SimpleConstraint))
    reactAsTarget
      = asum . map (\inert -> react inert c) . (.cs) <$> getInerts

    reactAsAux :: SolveM ()
    reactAsAux = filterInertsM $ \inert ->
      case react c inert of
        RProduct inert' -> False <$ pushConstraint' inert'
        RDischarge -> pure False
        RNoReaction -> pure True

--------------------------------------------------------------------------------

pushConstraint' :: GivenOrWanted SimpleConstraint -> SolveM ()
pushConstraint' = traverse_ pushConstraint . distributeMode <=< traverse canon
