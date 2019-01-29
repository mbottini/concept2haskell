module DataTypes.DistanceIntervalFrame where

data DistanceFrame = DistanceFrame {
    distance :: Int,
    heartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

