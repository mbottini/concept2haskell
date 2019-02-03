module DataTypes.TimeFrame where

import Data.Time.Clock
import Data.Word
import qualified Utils

data TimeFrame = TimeFrame {
    duration :: DiffTime,
    heartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseTimeFrame :: [Word8] -> TimeFrame
parseTimeFrame ws = TimeFrame {
    duration = Utils.parseDuration . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    strokesPerMinute = fromIntegral . (!! 3) $ ws
}
