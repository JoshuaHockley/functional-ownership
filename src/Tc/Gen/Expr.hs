module Tc.Gen.Expr where

import Ty
import Ty.Region
import Ty.Prim
import Ty.FromSyn
import Tc.Gen.Monad
import Tc.Gen.Env
import Tc.Gen.Context
import Tc.Gen.Liveness
import Tc.Gen.Instantiate
import Tc.Error
import Syn.Expr
import Util (impossible)

import Optics
import Control.Monad
import Data.Traversable
import Data.Foldable
import qualified Data.Set as Set

genExpr :: Expr () -> GenM (Typed (Expr Ty))
genExpr expr = do
  e' :- ty <- genExpr' expr.e
  pure $ expr {e = e', ann = ty} :- ty
  where
    genExpr' :: ExprKind () -> GenM (Typed (ExprKind Ty))
    genExpr' = \case
      ELit l -> pure $ ELit l :- litTy l
      EDataCon c -> fmap EDataCon <$> genDataCon c
      EMove v -> fmap EMove <$> genMove v
      ERef r -> fmap ERef <$> genRef r
      EDeref dr -> fmap EDeref <$> genDeref dr
      EDerefCall drc -> fmap EDerefCall <$> genDerefCall drc
      EApp app -> fmap EApp <$> genApp app
      ELam lam -> fmap ELam <$> genLam lam
      ELet let_ -> fmap ELet <$> genLet let_
      ECase case_ -> fmap ECase <$> genCase case_
      EIf if_ -> fmap EIf <$> genIf if_

litTy :: Lit -> Ty
litTy = TPrim . \case
  LBool _ -> PrimBool
  LInt _ -> PrimInt

genDataCon :: DataCon -> GenM (Typed DataCon)
genDataCon c = (c :-) <$> (instantiate =<< dataConTy c)

genMove :: Var -> GenM (Typed Var)
genMove v = do
  VarInfo {ty, rv} <- varInfo v
  case rv of
    Just rv' -> reportMove . MoveReport v rv' =<< newPoint
    Nothing -> throwTyError $ ETopLevelMove v
  (v :-) <$> instantiate ty

genRef :: Ref -> GenM (Typed Ref)
genRef Ref {v} = do
  VarInfo {ty, rv} <- varInfo v
  let lt = maybe LTStatic (TVar . TVUnification . (.utv)) rv
  (Ref v :-) . TRef lt <$> instantiate ty

genDeref :: Deref () -> GenM (Typed (Deref Ty))
genDeref Deref {e} = do
  (lt, tr) <- (,) <$> uvar <*> uvar
  e' :- te <- genExpr e
  raiseTyEq $ te :~: TRef lt tr
  raiseLiveness' lt
  raiseCopyable tr
  pure $ Deref e' :- tr

genDerefCall :: DerefCall () -> GenM (Typed (DerefCall Ty))
genDerefCall DerefCall {e} = do
  (lte, tx, tr, ltf, onceness) <- (,,,,)
    <$> uvar <*> uvar <*> uvar <*> uvar <*> uvar
  e' :- te <- genExpr e
  raiseTyEq $ te :~: TRef lte (TFn (tx :->: tr) ltf TMany)
  pure $ DerefCall e' :- TFn (tx :->: tr) (TRegionIntersection [lte, ltf]) onceness

genApp :: App () -> GenM (Typed (App Ty))
genApp App {f, x} = do
  (tr, ltf, onceness) <- (,,) <$> uvar <*> uvar <*> uvar
  f' :- tf <- genExpr f
  x' :- tx <- genExpr x
  raiseTyEq $ tf :~: TFn (tx :->: tr) ltf onceness
  raiseLiveness' ltf
  pure $ App f' x' :- tr

genLam :: Lam () -> GenM (Typed (Lam Ty))
genLam Lam {captures, pat, e} = do
  tx <- uvar
  captureCtx <- (mconcat <$>) . for captures $
    provideLocalVar . monoTyped <=< genMove
  (((e' :- te, innerTimeline), livenessConstraints), moved) <-
    listenMoves . listenLivenessConstraints . underTimeline $ do
      patCtx <- genPat (pat :- tx)
      withContext (patCtx <> captureCtx) $ genExpr e
  let lifetime = TRegionCut
        { above = innerTimeline.depth,
          r = TRegionIntersection
            [r | r :@: p <- livenessConstraints, p.timeline == innerTimeline]
        }
  let isOnce = any (`Set.member` moved) [(v, rv) | (v :- _, Just rv) <- captureCtx.vars]
  onceness <- if isOnce then pure TOnce else uvar
  pure $ Lam captures pat e' :- TFn (tx :->: te) lifetime onceness

genLet :: Let () -> GenM (Typed (Let Ty))
genLet Let {bindings, e} = case bindings of
  b : bs -> do
    (b', ctx) <- genLetBinding b
    Let bs' e' :- tlet <- withContext ctx $ genLet (Let bs e)
    pure $ Let (b' : bs') e' :- tlet
  [] -> do
    e' :- te <- genExpr e
    pure $ Let [] e' :- te

genCase :: Case () -> GenM (Typed (Case Ty))
genCase Case {e, branches} = do
  bks <- newBranchKeys
  tr <- uvar
  e' :- te <- genExpr e
  branches' <- for (zip branches bks) $ \(b, bk) ->
    underBranch bk $ genCaseBranch te tr b
  pure $ Case e' branches' :- tr

genCaseBranch :: Ty -> Ty -> CaseBranch () -> GenM (CaseBranch Ty)
genCaseBranch ta tr CaseBranch {pat, e} = do
  tbranch <- uvar
  ctx <- genPat (pat :- ta)
  e' :- te <- withContext ctx $ do
    raiseTyEq $ tbranch :~: tr
    genExpr e
  raiseTyEq $ te :~: tbranch
  pure $ CaseBranch pat e'

genIf :: If () -> GenM (Typed (If Ty))
genIf If {cond, t, e} = do
  (bk1, bk2) <- newBranchKeys <&> \case
    k1 : k2 : _ -> (k1, k2)
    _ -> impossible
  tr <- uvar
  cond' :- tcond <- genExpr cond
  t' :- tt <- underBranch bk1 $ genExpr t
  e' :- te <- underBranch bk2 $ genExpr e
  traverse_ raiseTyEq
    [ tcond :~: TPrim PrimBool,
      tt :~: tr,
      te :~: tr
    ]
  pure $ If cond' t' e' :- tr

--------------------------------------------------------------------------------

genLetBinding :: LetBinding () -> GenM (LetBinding Ty, Context)
genLetBinding = \case
  BVar b -> do
    (b', v) <- genVarBinding b
    (BVar b',) <$> provideLocalVar v
  BPat b -> do
    (b', ctx) <- genPatBinding b
    pure (BPat b', ctx)

genVarBinding :: VarBinding () -> GenM (VarBinding Ty, QTyped Var)
genVarBinding binding@VarBinding {rec, v, tyAnn, e} = do
  when (rec && not (isRecursionSafe e)) $
    throwTyError $ EInvalidRecursiveDefinition
  let tyAnn' = fmap qTyFromSyn tyAnn
  (outsideTy, insideTy, insideConstraints) <- case tyAnn' of
    Just ann -> do
      (ty, cs) <- rigidlyInstantiateTy ann
      pure (ann, ty, cs)
    Nothing -> do
      ty <- uvar
      pure (monoTy # ty, ty, [])
  let typedSelf = v :- outsideTy
  let ctx = (if rec then provideStaticVar typedSelf else mempty)
         <> provideConstraints insideConstraints
  e' :- _ <- withContext ctx $ do
    e' :- te <- genExpr e
    raiseTyEq $ te :~: insideTy
    pure $ e' :- te
  pure (binding & #e .~ e', typedSelf)
  where
    isRecursionSafe :: Expr () -> Bool
    isRecursionSafe Expr {e} = case e of
      ELam Lam {captures = []} -> True
      _ -> False

genPatBinding :: PatBinding () -> GenM (PatBinding Ty, Context)
genPatBinding binding@PatBinding {pat, e} = do
  e' :- te <- genExpr e
  ctx <- genPat (pat :- te)
  pure (binding & #e .~ e', ctx)

--------------------------------------------------------------------------------

genPat :: Typed Pat -> GenM Context
genPat (Pat {pat} :- tp) = case pat of
  PLit l -> raiseTyEq (tp :~: litTy l) *> pure mempty
  PVar v -> provideLocalVar $ monoTyped (v :- tp)
  PDecon decon -> genDeconPat (decon :- tp)

genDeconPat :: Typed DeconPat -> GenM Context
genDeconPat (DeconPat {underRef, con, pats} :- tp) = do
  refLt <- if underRef then Just <$> uvar else pure Nothing
  let applyRef = case refLt of
        Just lt -> TRef lt
        _ -> id
  (conTy, cs) <- instantiateTy =<< dataConTy con
  let argTys = map applyRef $ functionArgs conTy
      resTy = applyRef $ functionResult conTy
  raiseTyEq $ tp :~: resTy
  for_ refLt $ raiseLiveness'
  when (length argTys /= length pats) $
    throwTyError (EPatDeconMissmatchedArity con)
  ctx <- (mconcat <$>) $
    for (zip pats argTys) $ \(pat, argTy) ->
      genPat (pat :- argTy)
  pure $ provideConstraints cs <> ctx

--------------------------------------------------------------------------------

dataConTy :: DataCon -> GenM QTy
dataConTy c = lookupDataCon c
  >>= maybe (throwTyError (EDataConNotInScope c)) pure

varInfo :: Var -> GenM VarInfo
varInfo v = lookupVar v
  >>= maybe (throwTyError (EVarNotInScope v)) pure

--------------------------------------------------------------------------------

uvar :: GenM Ty
uvar = TVar . TVUnification <$> newUnificationVar

instantiate :: QTy -> GenM Ty
instantiate qTy = do
  (ty, cs) <- instantiateTy qTy
  for_ cs raiseConstraint
  pure ty

raiseTyEq :: TyEq Ty Ty -> GenM ()
raiseTyEq = raiseConstraint . CTyEq

raiseLiveness :: LivenessConstraint Ty -> GenM ()
raiseLiveness = raiseConstraint . CLiveness

raiseLiveness' :: Ty -> GenM ()
raiseLiveness' lt = raiseLiveness . (lt :@:) =<< newPoint

raiseCopyable :: Ty -> GenM ()
raiseCopyable = raiseConstraint . CCopyable . CopyableConstraint
