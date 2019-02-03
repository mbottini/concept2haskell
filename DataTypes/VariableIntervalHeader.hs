module DataTypes.VariableIntervalHeader where

import qualified DataTypes.WorkoutType as Wt
import qualified Utils
import Data.Word
import Data.Time.Clock

data VariableIntervalHeader = VariableIntervalHeader {
    workoutType :: Wt.WorkoutType,
    serialNumber :: Int,
    timeStamp :: UTCTime,
    userID :: Int,
    recordID :: Int,
    numSplits :: Int,
    splitSize :: Int,
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
    splitSize = Utils.parseBigEndian . Utils.grabChunk 20 2 $ ws,
    totalTime = Utils.parseDuration . Utils.grabChunk 22 4 $ ws,
    totalWorkDistance = Utils.parseBigEndian . Utils.grabChunk 26 4 $ ws
}

