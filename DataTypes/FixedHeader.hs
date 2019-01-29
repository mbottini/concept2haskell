module DataTypes.FixedHeader where

import qualified DataTypes.WorkoutType as Wt
import Data.Time.Clock

data FixedHeader = FixedHeader {
    workoutType :: Wt.WorkoutType,
    serialNumber :: Int,
    timeStamp :: UTCTime,
    recordID :: Int,
    totalDuration :: DiffTime,
    totalDistance :: Int,
    strokesPerMinute :: Int,
    splitInfo :: Int,
    splitSize :: Int
} deriving(Show)

