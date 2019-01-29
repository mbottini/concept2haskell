module DataTypes.TimeIntervalFrame where

import Data.Time.Clock

data DistanceFrame = DistanceFrame {
    distance :: Int,
    heartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

