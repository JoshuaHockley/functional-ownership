module Tc.Gen.Liveness where

import Ty
import Ty.Region
import Syn.Expr

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import Data.Maybe (maybeToList)
import Control.Applicative
import Data.Foldable
import Control.Monad.Except
import Optics
import GHC.Generics (Generic)

newtype LivenessMap = LivenessMap
  { map :: Map (Var, RegionUnificationTV) LivenessInfo
  }
  deriving (Generic, Show)

data LivenessInfo = LivenessInfo
  { moves :: [Point],
    drop :: Maybe Point
  }
  deriving (Generic, Show)

data MoveReport = MoveReport
  { v :: Var,
    rv :: RegionUnificationTV,
    p :: Point
  }
  deriving (Generic, Show)

data DropReport = DropReport
  { v :: Var,
    rv :: RegionUnificationTV,
    p :: Point
  }
  deriving (Generic, Show)

instance Monoid LivenessMap where
  mempty = LivenessMap M.empty

instance Semigroup LivenessMap where
  LivenessMap m1 <> LivenessMap m2
    = LivenessMap $ M.unionWith (<>) m1 m2

instance Monoid LivenessInfo where
  mempty = LivenessInfo {moves = [], drop = Nothing}

instance Semigroup LivenessInfo where
  i1 <> i2 = LivenessInfo
    { moves = i1.moves <> i2.moves,
      drop = i1.drop <|> i2.drop
    }

singletonMoveLiveness :: MoveReport -> LivenessMap
singletonMoveLiveness MoveReport {v, rv, p}
  = LivenessMap . M.singleton (v, rv) $ mempty & #moves .~ [p]

singletonDropLiveness :: DropReport -> LivenessMap
singletonDropLiveness DropReport {v, rv, p}
  = LivenessMap . M.singleton (v, rv) $ mempty & #drop .~ Just p

movedVars :: LivenessMap -> Set (Var, RegionUnificationTV)
movedVars = M.keysSet . M.filter (not . null . (.moves)) . (.map)

--------------------------------------------------------------------------------

data LivenessError
  = LEDoubleMove DoubleMove
  | LEOutOfTimelineMove OutOfTimelineMove
  deriving (Generic, Show)

data DoubleMove = DoubleMove
  { v :: Var,
    p1 :: Point,
    p2 :: Point
  }
  deriving (Generic, Show)

data OutOfTimelineMove = OutOfTimelineMove
  { v :: Var,
    p :: Point
  }
  deriving (Generic, Show)

consolidateLiveness :: LivenessMap -> Except LivenessError Constraints
consolidateLiveness (LivenessMap liveness)
  = mconcat . map simpleConstraint
    <$> traverse (uncurry consolidateLiveness') (M.toList liveness)
  where
    consolidateLiveness'
      :: (Var, RegionUnificationTV)
      -> LivenessInfo
      -> Except LivenessError SimpleConstraint
    consolidateLiveness' (v, RegionUnificationTV rv) LivenessInfo {moves, drop} = do
      for_ doubleMoves $
        throwError . LEDoubleMove
      for_ outOfTimelineMoves $
        throwError . LEOutOfTimelineMove
      pure constraint
      where
        doubleMoves :: [DoubleMove]
        doubleMoves
          = [DoubleMove v p1 p2 | p1 <- moves, p2 <- moves, p1 `pointPreceeds` p2, p1 /= p2]

        outOfTimelineMoves :: [OutOfTimelineMove]
        outOfTimelineMoves
          = [OutOfTimelineMove v p | Just d <- [drop], p <- moves, p.timeline /= d.timeline]

        livenessRegion :: Region
        livenessRegion = mconcat $ map endPointAt (maybeToList drop ++ moves)

        constraint :: SimpleConstraint
        constraint = CTyEq $ TVar (TVUnification rv) :~: TRegion livenessRegion
