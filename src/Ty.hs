module Ty where

import Ty.Prim
import Ty.Region
import qualified Syn.Decl as Syn
import qualified Syn.Ty as Syn
import Util ((.:))

import Optics
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.Optics (setOf)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)

data QTy = QTy
  { tvs :: Set RigidTV,
    constraints :: [SimpleConstraint],
    ty :: Ty
  }
  deriving (Generic, Show)

data Ty
  = TPrim PrimTy
  | TCon Syn.TyCon
  | TVar TyVar
  | TApp Ty (NonEmpty Ty)
  | TRef { lifetime :: Ty, ty :: Ty }
  | TFn { arrow :: ArrowTy, lifetime :: Ty, onceness :: Ty }
  | TRegion Region
  | TRegionCut { above :: TimelineDepth, r :: Ty }
  | TRegionIntersection [Ty]
  | TOnceness Onceness
  deriving (Generic, Eq, Show)

type TyCon = Syn.TyCon

data TyVar
  = TVRigid RigidTV
  | TVUnification UnificationTV
  deriving (Generic, Eq, Ord, Show)

data RigidTV = RigidTV
  { name :: Syn.TyVar,
    instanceId :: Maybe TyId
  }
  deriving (Generic, Eq, Ord, Show)

newtype UnificationTV = UnificationTV
  { id :: TyId
  }
  deriving (Generic, Eq, Ord, Show)

data ArrowTy = Ty :->: Ty
  deriving (Generic, Eq, Show)
infixr :->:

pattern LTStatic :: Ty
pattern LTStatic = TRegionIntersection []

data Onceness = Once | Many
  deriving (Generic, Eq, Show)

pattern TOnce :: Ty
pattern TOnce = TOnceness Once

pattern TMany :: Ty
pattern TMany = TOnceness Many

subTys :: Traversal' Ty Ty
subTys = traversalVL $ \f -> \case
  TApp t ts -> TApp <$> f t <*> traverse f ts
  TRef lt t -> TRef <$> f lt <*> f t
  TFn (a :->: b) lt once -> TFn <$> ((:->:) <$> f a <*> f b) <*> f lt <*> f once
  TRegionCut above r -> TRegionCut above <$> f r
  TRegionIntersection rs -> TRegionIntersection <$> traverse f rs
  t -> pure t

monoTy :: Prism' QTy Ty
monoTy = prism' asQTy toMonoTy
  where
    asQTy ty = QTy {tvs = Set.empty, constraints = [], ty}
    toMonoTy QTy {tvs, constraints, ty}
      | null tvs && null constraints = Just ty
      | otherwise = Nothing

functionView :: Ty -> (NonEmpty Ty)
functionView = \case
  TFn (a :->: b) _ _ -> NE.cons a (functionView b)
  t -> NE.singleton t

functionResult :: Ty -> Ty
functionResult = NE.last . functionView

functionArgs :: Ty -> [Ty]
functionArgs = NE.init . functionView

functionLifetimes :: Ty -> [Ty]
functionLifetimes = \case
  TFn (_ :->: t') lt _ -> lt : functionLifetimes t'
  _ -> []

functionOncenesses :: Ty -> [Ty]
functionOncenesses = \case
  TFn (_ :->: t') _ once -> once : functionOncenesses t'
  _ -> []

--------------------------------------------------------------------------------

data Constraints = Constraints
  { simples :: [SimpleConstraint],
    implics :: [ImplicConstraint]
  }
  deriving (Generic, Show)

data SimpleConstraint
  = CTyEq (TyEq Ty Ty)
  | CLiveness (LivenessConstraint Ty)
  | COutlives (OutlivesConstraint Ty)
  | CCopyable (CopyableConstraint Ty)
  | CClass ClassConstraint
  deriving (Generic, Show)

data TyEq t1 t2 = t1 :~: t2
  deriving (Generic, Show)

data LivenessConstraint ty = ty :@: Point
  deriving (Generic, Show)

data OutlivesConstraint ty = ty :>: ty
  deriving (Generic, Show)

newtype CopyableConstraint ty = CopyableConstraint
  { t :: ty
  }
  deriving (Generic, Show)

data ClassConstraint = ClassConstraint
  { class_ :: Class,
    tyArgs :: [Ty]
  }
  deriving (Generic, Show)

type Class = Syn.Class

data ImplicConstraint = ImplicConstraint
  { touchables :: [UnificationTV],
    antecedent :: [SimpleConstraint],
    consequent :: Constraints
  }
  deriving (Generic, Show)

instance Monoid Constraints where
  mempty = Constraints { simples = [], implics = [] }

instance Semigroup Constraints where
  c1 <> c2 = Constraints
    { simples = c1.simples <> c2.simples,
      implics = c1.implics <> c2.implics
    }

simpleConstraint :: SimpleConstraint -> Constraints
simpleConstraint c = Constraints { simples = [c], implics = [] }

implicConstraint :: ImplicConstraint -> Constraints
implicConstraint c = Constraints { simples = [], implics = [c] }

flattenedConstraints :: Traversal' Constraints SimpleConstraint
flattenedConstraints = traversalVL $ \f ->
  \Constraints {simples, implics} -> Constraints
    <$> traverse f simples
    <*> traverseOf (traversed % #consequent % flattenedConstraints) f implics

--------------------------------------------------------------------------------

newtype Axioms = Axioms
  { axioms :: [Axiom]
  }
  deriving (Generic, Show)

data Axiom = Axiom
  { tvs :: Set RigidTV,
    constraints :: [SimpleConstraint],
    axiom :: SimpleConstraint
  }
  deriving (Generic, Show)

--------------------------------------------------------------------------------

newtype TyId = TyId
  { id :: Integer
  }
  deriving (Generic, Eq, Ord, Show)

firstTyId :: TyId
firstTyId = TyId 0

nextTyId :: TyId -> TyId
nextTyId = #id %~ (+ 1)

assignRigidTVId :: TyId -> RigidTV -> RigidTV
assignRigidTVId id = #instanceId .~ Just id

--------------------------------------------------------------------------------

class HasTys a where
  tys :: Traversal' a Ty

instance HasTys Ty where
  tys = castOptic simple

instance HasTys QTy where
  tys = #constraints % traversed % tys `adjoin` #ty

instance HasTys Constraints where
  tys = #simples % traversed % tys `adjoin` #implics % traversed % tys

instance HasTys SimpleConstraint where
  tys = traversalVL $ \f -> \case
    CTyEq (t1 :~: t2) -> CTyEq .: (:~:) <$> f t1 <*> f t2
    CLiveness (r :@: p) -> CLiveness . (:@: p) <$> f r
    COutlives (r :>: s) -> COutlives .: (:>:) <$> f r <*> f s
    CCopyable copyable -> CCopyable <$> traverseOf #t f copyable
    CClass ClassConstraint {class_, tyArgs} ->
      CClass . ClassConstraint class_ <$> traverse f tyArgs

instance HasTys ImplicConstraint where
  tys = #antecedent % traversed % tys `adjoin` #consequent % tys

instance HasTys (Syn.Program Ty) where
  tys = traversed

instance HasTys a => HasTys [a] where
  tys = traversed % tys

tyVars :: HasTys a => Fold a TyVar
tyVars = tys % cosmosOf subTys % #_TVar

ftv :: HasTys a => a -> Set RigidTV
ftv = setOf (tyVars % #_TVRigid)

fuv :: HasTys a => a -> Set UnificationTV
fuv = setOf (tyVars % #_TVUnification)

applyTySub :: HasTys a => Map TyVar Ty -> a -> a
applyTySub sub = tys %~ apply
  where
    apply :: Ty -> Ty
    apply = transformOf subTys $ \case
      TVar tv | Just t' <- sub !? tv -> t'
      t -> t

applySingularTySub :: HasTys a => (TyVar, Ty) -> a -> a
applySingularTySub = applyTySub . uncurry M.singleton

--------------------------------------------------------------------------------

data GTyped ty a = a :- ty
  deriving (Generic, Functor, Show)
infix 3 :-

type Typed = GTyped Ty

type QTyped = GTyped QTy

typedTuple :: Iso (GTyped ty a) (GTyped ty' a') (a, ty) (a', ty')
typedTuple = iso (\(v :- ty) -> (v, ty)) (uncurry (:-))

typedValue :: Lens (GTyped ty a) (GTyped ty b) a b
typedValue = typedTuple % _1

typedTy :: Lens (GTyped ty a) (GTyped ty' a) ty ty'
typedTy = typedTuple % _2

monoTyped :: Typed a -> QTyped a
monoTyped = typedTy %~ (monoTy #)

--------------------------------------------------------------------------------

newtype RegionUnificationTV = RegionUnificationTV
  { utv :: UnificationTV
  }
  deriving (Generic, Eq, Ord, Show)
