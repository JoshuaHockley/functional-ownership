module Ty.Region where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Optics
import GHC.Generics (Generic)

newtype Region = Region
  { endPoints :: Set Point
  }
  deriving (Generic, Eq, Ord, Show)

data Point = Point
  { timeline :: Timeline,
    branchKeys :: Seq BranchKey,
    pointKey :: PointKey
  }
  deriving (Generic, Eq, Ord, Show)

data Timeline = Timeline
  { id :: TimelineId,
    depth :: TimelineDepth
  }
  deriving (Generic, Eq, Ord, Show)

newtype TimelineId = TimelineId
  { id :: Integer
  }
  deriving (Generic, Eq, Ord, Show)

newtype TimelineDepth = TimelineDepth
  { depth :: Integer
  }
  deriving (Generic, Eq, Ord, Show)

data BranchKey = BranchKey
  { branchPointKey :: PointKey,
    branchKey :: Integer
  }
  deriving (Generic, Eq, Ord, Show)

newtype PointKey = PointKey
  { key :: Integer
  }
  deriving (Generic, Eq, Ord, Show)

instance Monoid Region where
  mempty = totalRegion

instance Semigroup Region where
  (<>) = regionIntersect

totalRegion :: Region
totalRegion = Region Set.empty

regionTimelines :: Region -> Set Timeline
regionTimelines = Set.map (.timeline) . (.endPoints)

regionIncludes :: Region -> Point -> Bool
r `regionIncludes` p = not $ any (`pointPreceeds` p) r.endPoints

regionOutlives :: Region -> Region -> Bool
r `regionOutlives` s = all (\pr -> any (`pointPreceeds` pr) s.endPoints) r.endPoints

regionIntersect :: Region -> Region -> Region
regionIntersect r s = Region $ Set.union r.endPoints s.endPoints

cutRegionAbove :: TimelineDepth -> Region -> Region
cutRegionAbove d = filterRegionEndpoints (\p -> p.timeline.depth < d)

cutRegionBelowEq :: TimelineDepth -> Region -> Region
cutRegionBelowEq d = filterRegionEndpoints (\p -> p.timeline.depth >= d)

filterRegionEndpoints :: (Point -> Bool) -> Region -> Region
filterRegionEndpoints pred = #endPoints %~ Set.filter pred

endPointAt :: Point -> Region
endPointAt = Region . Set.singleton

pointPreceeds :: Point -> Point -> Bool
p `pointPreceeds` q
  | p.timeline /= q.timeline = False
  | or (Seq.zipWith isParallel p.branchKeys q.branchKeys) = False
  | otherwise = p.pointKey <= q.pointKey
  where
    isParallel k1 k2
      = k1.branchPointKey == k2.branchPointKey
      && k1.branchKey /= k2.branchKey

--------------------------------------------------------------------------------

rootTimeline :: Timeline
rootTimeline = Timeline
  { id = TimelineId 0,
    depth = TimelineDepth 0
  }

nestTimelineWithId :: TimelineId -> Timeline -> Timeline
nestTimelineWithId id tl = Timeline
  { id,
    depth = tl.depth & #depth %~ (+ 1)
  }

firstTimelineId :: TimelineId
firstTimelineId = nextTimelineId rootTimeline.id

nextTimelineId :: TimelineId -> TimelineId
nextTimelineId = #id %~ (+ 1)

firstPointKey :: PointKey
firstPointKey = PointKey 0

nextPointKey :: PointKey -> PointKey
nextPointKey = #key %~ (+ 1)

branchKeysFor :: PointKey -> [BranchKey]
branchKeysFor p = map (BranchKey p) [0..]
