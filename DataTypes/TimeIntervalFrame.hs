module DataTypes.TimeIntervalFrame where

import Data.Word
import qualified Utils

data TimeIntervalFrame = TimeIntervalFrame {
    distance :: Int,
    heartRate :: Int,
    restHeartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseTimeIntervalFrame :: [Word8] -> TimeIntervalFrame
parseTimeIntervalFrame ws = TimeIntervalFrame {
    distance = Utils.parseBigEndian . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    restHeartRate = fromIntegral . (!! 3) $ ws,
    strokesPerMinute = fromIntegral . (!! 4) $ ws
}
