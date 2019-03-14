{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.FixedTimeHeader where

import qualified DataTypes.WorkoutType as Wt
import qualified Utils
import Data.Time
import Data.Word
import Data.Aeson
import Data.Text

data FixedTimeHeader = FixedTimeHeader {
    workoutType :: Wt.WorkoutType,
    serialNumber :: Int,
    timeStamp :: LocalTime,
    userID :: Int,
    recordID :: Int,
    totalDuration :: DiffTime,
    totalDistance :: Int,
    strokesPerMinute :: Int,
    splitInfo :: Int,
    splitSize :: DiffTime
} deriving(Show)

parseFixedTimeHeader :: [Word8] -> FixedTimeHeader
parseFixedTimeHeader lst = FixedTimeHeader {
    workoutType = Wt.parseWorkoutType . fromIntegral . (!! 1) $ lst,
    serialNumber = Utils.parseBigEndian . Utils.grabChunk 4 4 $ lst,
    timeStamp = Utils.parseDateStamp . Utils.grabChunk 8 4 $ lst,
    userID = Utils.parseBigEndian . Utils.grabChunk 12 2 $ lst,
    recordID = fromIntegral . (!! 18) $ lst,
    totalDuration = Utils.parseDuration . Utils.grabChunk 22 2 $ lst,
    totalDistance = Utils.parseBigEndian . Utils.grabChunk 24 4 $ lst,
    strokesPerMinute = fromIntegral . (!! 28) $ lst,
    splitInfo = fromIntegral . (!! 29) $ lst,
    splitSize = Utils.parseDuration . Utils.grabChunk 30 2 $ lst
}

instance ToJSON FixedTimeHeader where
    toJSON h = object [
        "workout_type" .= String "FixedTimeSplits" ,
        "date" .= String (pack . show . timeStamp $ h),
        "distance" .= Number (Utils.intToScientific . totalDistance $ h),
        "time" .= Number (Utils.tenthsToScientific . totalDuration $ h),
        "weight_class" .= String "H",
        "stroke_rate" .= Number (Utils.intToScientific . strokesPerMinute $ h)]


