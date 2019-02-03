module DataTypes.TimeIntervalFrame where

import Data.Time.Clock
import Data.Word
import qualified Utils

data TimeIntervalFrame = TimeIntervalFrame {
    duration :: DiffTime,
    heartRate :: Int,
    restHeartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseTimeIntervalFrame :: [Word8] -> TimeIntervalFrame
parseTimeIntervalFrame ws = TimeIntervalFrame {
    duration = Utils.parseDuration . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    restHeartRate = fromIntegral . (!! 3) $ ws,
    strokesPerMinute = fromIntegral . (!! 4) $ ws
}
