module DataTypes.DistanceIntervalHeader where

import Data.Time.Clock
import Data.Word
import qualified DataTypes.WorkoutType as Wt
import qualified Utils

data DistanceIntervalHeader = DistanceIntervalHeader {
    workoutType :: Wt.WorkoutType,
    serialNumber :: Int,
    timeStamp :: UTCTime,
    userID :: Int,
    recordID :: Int,
    numSplits :: Int,
    splitSize :: Int,
    restTime :: DiffTime,
    totalTime :: DiffTime,
    totalRestDistance :: Int
} deriving(Show)

parseDistanceIntervalHeader :: [Word8] -> DistanceIntervalHeader
parseDistanceIntervalHeader ws = DistanceIntervalHeader {
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
