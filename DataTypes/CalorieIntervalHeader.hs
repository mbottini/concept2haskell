{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.CalorieIntervalHeader where

import Data.Time
import Data.Word
import qualified DataTypes.WorkoutType as Wt
import qualified Utils
import Data.Aeson
import Data.Text

data CalorieIntervalHeader = CalorieIntervalHeader {
    workoutType :: Wt.WorkoutType,
    serialNumber :: Int,
    timeStamp :: LocalTime,
    userID :: Int,
    recordID :: Int,
    numSplits :: Int,
    splitSize :: Int,
    restTime :: DiffTime,
    totalTime :: DiffTime,
    totalRestDistance :: Int
} deriving(Show)

parseCalorieIntervalHeader :: [Word8] -> CalorieIntervalHeader
parseCalorieIntervalHeader ws = CalorieIntervalHeader {
    workoutType = Wt.parseWorkoutType . fromIntegral . (!! 1) $ ws,
    serialNumber = Utils.parseBigEndian . Utils.grabChunk 4 4 $ ws,
    timeStamp = Utils.parseDateStamp . Utils.grabChunk 8 4 $ ws,
    userID = Utils.parseBigEndian . Utils.grabChunk 12 2 $ ws,
    recordID = fromIntegral . (!! 18) $ ws,
    numSplits = fromIntegral . (!! 19) $ ws,
    splitSize = Utils.parseBigEndian . Utils.grabChunk 20 2 $ ws,
    restTime = Utils.parseSecs . Utils.grabChunk 22 2 $ ws,
    totalTime = Utils.parseDuration . Utils.grabChunk 24 4 $ ws,
    totalRestDistance = Utils.parseBigEndian . Utils.grabChunk 28 2 $ ws
}

instance ToJSON CalorieIntervalHeader where
    toJSON h = object [
        "type" .= String "rower",
        "workout_type" .= String "FixedCalorieInterval",
        "date" .= String (pack . Utils.formatTimeStamp . timeStamp $ h),
        "time" .= Number (Utils.tenthsToScientific . totalTime $ h),
        "rest_time" .= Number (Utils.tenthsToScientific . 
                               Utils.multiplyInterval (numSplits h) .
                               restTime $ h),
        "rest_distance" .= Number (Utils.intToScientific . totalRestDistance $ h),
        "weight_class" .= String "H"]
