{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.VariableIntervalHeader where

import qualified DataTypes.WorkoutType as Wt
import qualified Utils
import Data.Word
import Data.Time
import Data.Aeson
import Data.Text

data VariableIntervalHeader = VariableIntervalHeader {
    workoutType :: Wt.WorkoutType,
    serialNumber :: Int,
    timeStamp :: LocalTime,
    userID :: Int,
    recordID :: Int,
    numSplits :: Int,
    totalTime :: DiffTime,
    totalWorkDistance :: Int
} deriving(Show)

parseVariableIntervalHeader :: [Word8] -> VariableIntervalHeader
parseVariableIntervalHeader ws = VariableIntervalHeader {
    workoutType = Wt.parseWorkoutType . fromIntegral . (!! 1) $ ws,
    serialNumber = Utils.parseBigEndian . Utils.grabChunk 4 4 $ ws,
    timeStamp = Utils.parseDateStamp . Utils.grabChunk 8 4 $ ws,
    userID = Utils.parseBigEndian . Utils.grabChunk 12 2 $ ws,
    recordID = fromIntegral . (!! 18) $ ws,
    numSplits = fromIntegral . (!! 19) $ ws,
    totalTime = Utils.parseDuration . Utils.grabChunk 20 4 $ ws,
    totalWorkDistance = Utils.parseBigEndian . Utils.grabChunk 24 4 $ ws
}

instance ToJSON VariableIntervalHeader where
    toJSON h = object [
        "workout_type" .= String "VariableInterval" ,
        "date" .= String (pack . show . timeStamp $ h),
        "distance" .= Number (Utils.intToScientific . totalWorkDistance $ h),
        "weight_class" .= String "H"]
