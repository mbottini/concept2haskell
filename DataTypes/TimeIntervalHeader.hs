{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.TimeIntervalHeader where

import Data.Time
import Data.Word
import Data.Aeson
import Data.Text
import qualified DataTypes.WorkoutType as Wt
import qualified Utils



data TimeIntervalHeader = TimeIntervalHeader {
    workoutType :: Wt.WorkoutType,
    serialNumber :: Int,
    timeStamp :: LocalTime,
    userID :: Int,
    recordID :: Int,
    numSplits :: Int,
    splitSize :: DiffTime,
    restTime :: DiffTime,
    totalDistance :: Int,
    totalRestDistance :: Int
} deriving(Show)

parseTimeIntervalHeader :: [Word8] -> TimeIntervalHeader
parseTimeIntervalHeader ws = TimeIntervalHeader {
    workoutType = Wt.parseWorkoutType . fromIntegral . (!! 1) $ ws,
    serialNumber = Utils.parseBigEndian . Utils.grabChunk 4 4 $ ws,
    timeStamp = Utils.parseDateStamp . Utils.grabChunk 8 4 $ ws,
    userID = Utils.parseBigEndian . Utils.grabChunk 12 2 $ ws,
    recordID = fromIntegral . (!! 18) $ ws,
    numSplits = fromIntegral . (!! 19) $ ws,
    splitSize = Utils.parseDuration . Utils.grabChunk 20 2 $ ws,
    restTime = Utils.parseSecs . Utils.grabChunk 22 2 $ ws,
    totalDistance = Utils.parseBigEndian . Utils.grabChunk 24 4 $ ws,
    totalRestDistance = Utils.parseBigEndian . Utils.grabChunk 28 2 $ ws
}

instance ToJSON TimeIntervalHeader where
    toJSON h = object [
        "type" .= String "rower",
        "workout_type" .= String "FixedTimeInterval" ,
        "date" .= String (pack . show . timeStamp $ h),
        "distance" .= Number (Utils.intToScientific . totalDistance $ h),
        "rest_distance" .= Number (Utils.intToScientific . totalRestDistance $ h),
        "rest_time" .= Number (Utils.tenthsToScientific . 
                               Utils.multiplyInterval (numSplits h) .
                               restTime $ h),
        "weight_class" .= String "H"
        ]
