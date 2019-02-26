module DataTypes.CalorieIntervalFrame where

import qualified Utils
import Data.Word
import Data.Time.Clock

data CalorieIntervalFrame = CalorieIntervalFrame {
    duration :: DiffTime,
    heartRate :: Int,
    restHeartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseCalorieIntervalFrame :: [Word8] -> CalorieIntervalFrame
parseCalorieIntervalFrame ws = CalorieIntervalFrame {
    duration = Utils.parseDuration . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    restHeartRate = fromIntegral . (!! 3) $ ws,
    strokesPerMinute = fromIntegral . (!! 4) $ ws
}

