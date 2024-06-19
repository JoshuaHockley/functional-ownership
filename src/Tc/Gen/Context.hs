module Tc.Gen.Context where

import Ty
import Tc.Gen.Monad
import Tc.Gen.Env
import Tc.Gen.Liveness
import Syn.Expr

import Data.Foldable
import GHC.Generics (Generic)

data Context = Context
  { vars :: [(QTyped Var, Maybe RegionUnificationTV)],
    constraints :: [SimpleConstraint]
  }
  deriving (Generic, Show)

instance Monoid Context where
  mempty = Context {vars = [], constraints = []}

instance Semigroup Context where
  c1 <> c2 = Context
    { vars = c1.vars <> c2.vars,
      constraints = c1.constraints <> c2.constraints
    }

provideVar :: (QTyped Var, Maybe RegionUnificationTV) -> Context
provideVar (v, rv) = Context {vars = [(v, rv)], constraints = []}

provideStaticVar :: QTyped Var -> Context
provideStaticVar = provideVar . (, Nothing)

provideLocalVar :: QTyped Var -> GenM Context
provideLocalVar v = do
  rv <- Just . RegionUnificationTV <$> newUnificationVar
  pure $ provideVar (v, rv)

provideConstraints :: [SimpleConstraint] -> Context
provideConstraints cs = Context {vars = [], constraints = cs}

withContext :: Context -> GenM (Typed a) -> GenM (Typed a)
withContext Context {vars, constraints} g = do
  (x :- ty, cs) <- catchConstraints $
    extendEnv (envOfVars vars) g

  let cs'
        | null constraints = cs
        | otherwise = implicConstraint $ ImplicConstraint
          { touchables = undefined, -- TODO
            antecedent = constraints,
            consequent = cs
          }
  raiseConstraints cs'

  scopeEnd <- newPoint
  for_ [(v, rv) | (v :- _, Just rv) <- vars] $ \(v, rv) ->
    reportDrop $ DropReport v rv scopeEnd

  pure $ x :- ty
