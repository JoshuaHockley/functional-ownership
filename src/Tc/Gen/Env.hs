module Tc.Gen.Env where

import Ty
import Syn.Expr
import qualified Builtins

import Data.Map ((!?))
import qualified Data.Map as M
import Data.List (singleton)
import Optics
import GHC.Generics (Generic)

data Env = Env
  { vars :: M.Map Var VarInfo,
    dataCons :: M.Map DataCon QTy
  }
  deriving (Generic, Show)

instance Monoid Env where
  mempty = emptyEnv

instance Semigroup Env where
  e1 <> e2 = Env
    { vars = e1.vars <> e2.vars,
      dataCons = e1.dataCons <> e2.dataCons
    }

data VarInfo = VarInfo
  { ty :: QTy,
    rv :: Maybe RegionUnificationTV
  }
  deriving (Generic, Show)

emptyEnv :: Env
emptyEnv = Env {vars = M.empty, dataCons = M.empty}

initialEnv :: Env
initialEnv
  = envOfStaticVars Builtins.builtinVars
  <> envOfDataCons Builtins.builtinDataCons

envLookupVar :: Var -> Env -> Maybe VarInfo
envLookupVar v e = e.vars !? v

envLookupDataCon :: DataCon -> Env -> Maybe QTy
envLookupDataCon v e = e.dataCons !? v

envOfVars :: [(QTyped Var, Maybe RegionUnificationTV)] -> Env
envOfVars vs = (mempty &) . (#vars .~) . M.fromList $
  map (\(v :- ty, r) -> (v, VarInfo ty r)) vs

envOfStaticVars :: [QTyped Var] -> Env
envOfStaticVars = envOfVars . map (, Nothing)

envOfStaticVar :: QTyped Var -> Env
envOfStaticVar = envOfStaticVars . singleton

envOfDataCons :: [QTyped DataCon] -> Env
envOfDataCons cs = mempty & #dataCons .~ M.fromList (map (^. typedTuple) cs)
