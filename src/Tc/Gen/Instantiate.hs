module Tc.Gen.Instantiate where

import Ty
import Tc.Gen.Monad

import qualified Data.Map as M
import Data.Traversable
import Data.Foldable

instantiateTy :: QTy -> GenM (Ty, [SimpleConstraint])
instantiateTy = genericInstantiateTy ((TVUnification <$>) . const newUnificationVar)

rigidlyInstantiateTy :: QTy -> GenM (Ty, [SimpleConstraint])
rigidlyInstantiateTy = genericInstantiateTy ((TVRigid <$>) . rigidlyInstantiateTV)

genericInstantiateTy :: (RigidTV -> GenM TyVar) -> QTy -> GenM (Ty, [SimpleConstraint])
genericInstantiateTy instantiator QTy {tvs, constraints, ty} = do
  instantiation <- (M.fromList <$>) $
    for (toList tvs) $ \tv ->
      (TVRigid tv,) . TVar <$> instantiator tv
  let ty' = applyTySub instantiation ty
      constraints' = applyTySub instantiation constraints
  pure (ty', constraints')
