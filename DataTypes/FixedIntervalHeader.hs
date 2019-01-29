module DataTypes.FixedIntervalHeader where

import qualified DataTypes.WorkoutType as Wt
import Data.Time.Clock

data FixedIntervalHeader = FixedIntervalHeader {
    workoutType :: Wt.WorkoutType,
    serialNumber :: Int,
    timeStamp :: UTCTime,
    recordID :: Int,
    numSplits :: Int,
    splitSize :: Int,
    restTime :: DiffTime,
    totalTime :: DiffTime,
    totalRestDistance :: Int
} deriving(Show)

