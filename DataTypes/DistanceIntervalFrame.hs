module DataTypes.DistanceIntervalFrame where

import qualified Utils
import Data.Word

data DistanceIntervalFrame = DistanceIntervalFrame {
    distance :: Int,
    heartRate :: Int,
    restHeartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseDistanceIntervalFrame :: [Word8] -> DistanceIntervalFrame
parseDistanceIntervalFrame ws = DistanceIntervalFrame {
    distance = Utils.parseBigEndian . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    restHeartRate = fromIntegral . (!! 3) $ ws,
    strokesPerMinute = fromIntegral . (!! 4) $ ws
}

