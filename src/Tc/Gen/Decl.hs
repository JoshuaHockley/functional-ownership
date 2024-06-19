module Tc.Gen.Decl where

import Ty
import Ty.FromSyn
import Tc.Gen.Monad
import Tc.Gen.Expr
import Tc.Gen.Env
import Tc.Error
import Syn.Decl
import Syn.Expr

import Data.Bifunctor
import Data.Traversable

genProgram :: Program () -> GenM (Program Ty)
genProgram Program {decls, e} = do
  (env', decls') <- mapAccumM accDecl initialEnv decls
  e' :- _ <- extendEnv env' $ genExpr e
  pure $ Program decls' e'
  where
    accDecl :: Env -> Decl () -> GenM (Env, Decl Ty)
    accDecl env d = do
      (d', env') <- extendEnv env $ genDecl d
      pure (env' <> env, d')

genDecl :: Decl () -> GenM (Decl Ty, Env)
genDecl decl = do
  (d', e) <- case decl.d of
    DBinding b -> first DBinding <$> genBindingDecl b
    DData dataDecl -> first DData <$> genDataDecl dataDecl
    DClass _ -> undefined
    DInstance _ -> undefined
  pure (decl {d = d'}, e)

genBindingDecl :: VarBinding () -> GenM (VarBinding Ty, Env)
genBindingDecl = (second envOfStaticVar <$>) . genVarBinding

genDataDecl :: DataDecl -> GenM (DataDecl, Env)
genDataDecl dataDecl@DataDecl {conSigs} = do
  conSigs' <- traverse genDataConDecl conSigs
  pure (dataDecl, envOfDataCons conSigs')
  where
    genDataConDecl :: DataConSig -> GenM (QTyped DataCon)
    genDataConDecl DataConSig {con, ty}
      | isValidDataConTy ty' = pure (con :- qTyFromSyn ty)
      | otherwise = throwTyError $ EInvalidDataConTy ty'
      where
        ty' = qTyFromSyn ty
    isValidDataConTy :: QTy -> Bool
    isValidDataConTy QTy {tvs, constraints, ty}
      | TFn (_ :->: innerConTy) lt once <- ty,
        lt /= LTStatic
        || once /= TMany
        || any (/= LTStatic) (functionLifetimes innerConTy)
        || any (/= TOnce) (functionOncenesses innerConTy)
        = False
      | otherwise = True
