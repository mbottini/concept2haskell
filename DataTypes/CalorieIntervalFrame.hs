{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.CalorieIntervalFrame where

import qualified Utils
import Data.Word
import Data.Time.Clock
import Data.Aeson

data CalorieIntervalFrame = CalorieIntervalFrame {
    duration :: DiffTime,
    heartRate :: Int,
    restHeartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseCalorieIntervalFrame :: [Word8] -> CalorieIntervalFrame
parseCalorieIntervalFrame ws = CalorieIntervalFrame {
    duration = Utils.parseDuration . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    restHeartRate = fromIntegral . (!! 3) $ ws,
    strokesPerMinute = fromIntegral . (!! 4) $ ws
}

instance ToJSON CalorieIntervalFrame where
    toJSON cif = object [
        "type" .= String "calorie",
        "time" .= Number (Utils.tenthsToScientific . duration $ cif),
        "stroke_rate" .= Number (Utils.intToScientific . strokesPerMinute $ cif)
        ]

getMetersFromCif :: Int -> CalorieIntervalFrame -> Int
getMetersFromCif cals cif = Utils.calsToMeters (duration cif) cals

insertDistance :: Int -> CalorieIntervalFrame -> Value
insertDistance cals cif = Utils.mergeObjects (toJSON cif)
                                            (object ["distance" .= d])
    where d = Utils.calsToMeters (duration cif) cals
