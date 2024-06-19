module Tc.Gen.Monad where

import Ty
import Ty.Region
import Tc.Gen.Env
import Tc.Gen.Liveness
import Tc.Error
import Syn.Expr

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import Data.Bifunctor
import Control.Monad.RWS
import Control.Monad.Except
import Optics
import Optics.State.Operators
import GHC.Generics (Generic)

newtype GenM a = GenM
  { gen :: RWST GenEnv GenOutput GenState (Except TyError) a
  }
  deriving Generic
  deriving newtype (Functor, Applicative, Monad)

runGenM :: GenM a -> Except TyError (a, GenOutput)
runGenM GenM {gen} = evalRWST gen initGenEnv initGenState

lookupVar :: Var -> GenM (Maybe VarInfo)
lookupVar v = GenM $ envLookupVar v . (.tyEnv) <$> ask

lookupDataCon :: DataCon -> GenM (Maybe QTy)
lookupDataCon c = GenM $ envLookupDataCon c . (.tyEnv) <$> ask

extendEnv :: Env -> GenM a -> GenM a
extendEnv e = #gen %~ local (#tyEnv %~ (e <>))

underTimeline :: GenM a -> GenM (a, Timeline)
underTimeline GenM {gen} = GenM $ do
  id <- newTimelineId
  timeline' <- nestTimelineWithId id . (.timeline) <$> ask
  let nestTimeline = #timeline .~ timeline'
      forgetBranches = #branchKeys .~ unbranched
  (, timeline') <$>
    local (nestTimeline . forgetBranches) gen

underBranch :: BranchKey -> GenM a -> GenM a
underBranch k = #gen %~ local (#branchKeys %~ (Seq.|> k))

raiseConstraints :: Constraints -> GenM ()
raiseConstraints = GenM . tell . outputConstraints

raiseConstraint :: SimpleConstraint -> GenM ()
raiseConstraint = raiseConstraints . simpleConstraint

listenConstraints :: GenM a -> GenM (a, Constraints)
listenConstraints = #gen %~ (second (.constraints) <$>) . listen

listenLivenessConstraints :: GenM a -> GenM (a, [LivenessConstraint Ty])
listenLivenessConstraints = (second extract <$>) . listenConstraints
  where
    extract = toListOf (flattenedConstraints % #_CLiveness)

mapConstraints :: (Constraints -> Constraints) -> GenM a -> GenM a
mapConstraints f = #gen %~ mapRWST (mapped % _3 % #constraints %~ f)

catchConstraints :: GenM a -> GenM (a, Constraints)
catchConstraints = #gen %~ mapRWST (fmap catch)
  where
    catch (x, s, out)
      = ((x, out.constraints), s, out & #constraints .~ mempty)

reportMove :: MoveReport -> GenM ()
reportMove = GenM . tell . outputLiveness . singletonMoveLiveness

reportDrop :: DropReport -> GenM ()
reportDrop = GenM . tell . outputLiveness . singletonDropLiveness

listenLiveness :: GenM a -> GenM (a, LivenessMap)
listenLiveness = #gen %~ (second (.liveness) <$>) . listen

listenMoves :: GenM a -> GenM (a, Set (Var, RegionUnificationTV))
listenMoves = ((second movedVars) <$>) . listenLiveness

newUnificationVar :: GenM UnificationTV
newUnificationVar = GenM $ UnificationTV <$> newTyId

rigidlyInstantiateTV :: RigidTV -> GenM RigidTV
rigidlyInstantiateTV tv = GenM $
  assignRigidTVId <$> newTyId <*> pure tv

newPoint :: GenM Point
newPoint = GenM $ Point
  <$> ((.timeline) <$> ask)
  <*> ((.branchKeys) <$> ask)
  <*> newPointKey

newBranchKeys :: GenM [BranchKey]
newBranchKeys = GenM $ branchKeysFor <$> newPointKey

throwTyError :: TyError -> GenM a
throwTyError = GenM . throwError

--------------------------------------------------------------------------------

data GenEnv = GenEnv
  { tyEnv :: Env,
    timeline :: Timeline,
    branchKeys :: Seq BranchKey
  }
  deriving (Generic, Show)

initGenEnv :: GenEnv
initGenEnv = GenEnv
  { tyEnv = emptyEnv,
    timeline = rootTimeline,
    branchKeys = unbranched
  }

unbranched :: Seq BranchKey
unbranched = Seq.empty

--------------------------------------------------------------------------------

data GenOutput = GenOutput
  { constraints :: Constraints,
    liveness :: LivenessMap
  }
  deriving (Generic, Show)

instance Monoid GenOutput where
  mempty = GenOutput
    { constraints = mempty,
      liveness = mempty
    }

instance Semigroup GenOutput where
  o1 <> o2 = GenOutput
    { constraints = o1.constraints <> o2.constraints,
      liveness = o1.liveness <> o2.liveness
    }

outputConstraints :: Constraints -> GenOutput
outputConstraints = (mempty &) . (#constraints .~)

outputLiveness :: LivenessMap -> GenOutput
outputLiveness = (mempty &) . (#liveness .~)

--------------------------------------------------------------------------------

data GenState = GenState
  { nextTyId :: TyId,
    nextTimelineId :: TimelineId,
    nextPointKey :: PointKey
  }
  deriving (Generic, Show)

initGenState :: GenState
initGenState = GenState
  { nextTyId = firstTyId,
    nextTimelineId = firstTimelineId,
    nextPointKey = firstPointKey
  }

newTyId :: MonadState GenState m => m TyId
newTyId = newIdWith #nextTyId nextTyId

newTimelineId :: MonadState GenState m => m TimelineId
newTimelineId = newIdWith #nextTimelineId nextTimelineId

newPointKey :: MonadState GenState m => m PointKey
newPointKey = newIdWith #nextPointKey nextPointKey

newIdWith :: MonadState GenState m => Lens' GenState id -> (id -> id) -> m id
newIdWith o next = do
  id <- use o
  o %= next
  pure id
