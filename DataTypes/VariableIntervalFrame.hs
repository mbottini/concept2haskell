module DataTypes.VariableIntervalFrame where

import Data.Time.Clock

data VariableIntervalFrame = VariableIntervalFrame {
    splitType :: Int,
    strokesPerMinute :: Int,
    workIntervalTime :: DiffTime,
    workIntervalDistance :: Int,
    heartRate :: Int,
    restHeartRate :: Int,
    intervalRestTime :: DiffTime,
    intervalRestDistance :: Int
} deriving(Show)

