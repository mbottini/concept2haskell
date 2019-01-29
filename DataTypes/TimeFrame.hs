module DataTypes.TimeFrame where

import Data.Time.Clock

data TimeFrame = TimeFrame {
    duration :: DiffTime,
    heartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

