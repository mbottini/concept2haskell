module DataTypes.DistanceIntervalFrame where

import qualified Utils
import Data.Word
import Data.Time.Clock

data DistanceIntervalFrame = DistanceIntervalFrame {
    duration :: DiffTime,
    heartRate :: Int,
    restHeartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseDistanceIntervalFrame :: [Word8] -> DistanceIntervalFrame
parseDistanceIntervalFrame ws = DistanceIntervalFrame {
    duration = Utils.parseDuration . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    restHeartRate = fromIntegral . (!! 3) $ ws,
    strokesPerMinute = fromIntegral . (!! 4) $ ws
}

