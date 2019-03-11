{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.VariableIntervalFrame where

import Data.Time.Clock
import Data.Word
import Data.Aeson
import qualified Utils

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

parseVariableIntervalFrame :: [Word8] -> VariableIntervalFrame
parseVariableIntervalFrame ws = VariableIntervalFrame {
    splitType = fromIntegral . head $ ws,
    strokesPerMinute = fromIntegral . (!! 1) $ ws,
    workIntervalTime = Utils.parseDuration . Utils.grabChunk 2 4 $ ws,
    workIntervalDistance = Utils.parseBigEndian . Utils.grabChunk 6 4 $ ws,
    heartRate = fromIntegral . (!! 10) $ ws,
    restHeartRate = fromIntegral . (!! 11) $ ws,
    intervalRestTime = Utils.parseSecs . Utils.grabChunk 12 2 $ ws,
    intervalRestDistance = Utils.parseBigEndian . Utils.grabChunk 14 2 $ ws
}

instance ToJSON VariableIntervalFrame where
    toJSON vif = object [
        "type" .= String "time", -- It's always time, no matter what workout you do.
        "time" .= Number (Utils.tenthsToScientific . workIntervalTime $ vif),
        "distance" .= Number (Utils.intToScientific . workIntervalDistance $ vif),
        "rest_time" .= Number (Utils.tenthsToScientific . intervalRestTime $ vif),
        "rest_distance" .= Number (Utils.intToScientific . intervalRestDistance $ vif),
        "stroke_rate" .= Number (Utils.intToScientific . strokesPerMinute $ vif)]
        
getRestTimeSPM :: VariableIntervalFrame -> (DiffTime, Int)
getRestTimeSPM vif = (intervalRestTime vif, strokesPerMinute vif)