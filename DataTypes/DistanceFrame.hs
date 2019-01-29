module DataTypes.DistanceFrame where

data DistanceFrame = DistanceFrame {
    distance :: Int,
    heartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

