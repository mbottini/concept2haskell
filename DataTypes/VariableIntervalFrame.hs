module DataTypes.VariableIntervalFrame where

import Data.Time.Clock
import Data.Word
import qualified Utils

data VariableIntervalFrame = VariableIntervalFrame {
    splitType :: Int,
    strokesPerMinute :: Int,
    workIntervalTime :: DiffTime,
    workIntervalDistance :: Int,
    heartRate :: Int,
    restHeartRate :: Int,
    intervalRestTime :: DiffTime,
    intervalRestDistance :: Int
} deriving(Show)

parseVariableIntervalFrame :: [Word8] -> VariableIntervalFrame
parseVariableIntervalFrame ws = VariableIntervalFrame {
    splitType = fromIntegral . head $ ws,
    strokesPerMinute = fromIntegral . (!! 1) $ ws,
    workIntervalTime = Utils.parseDuration . Utils.grabChunk 2 4 $ ws,
    workIntervalDistance = Utils.parseBigEndian . Utils.grabChunk 6 4 $ ws,
    heartRate = fromIntegral . (!! 10) $ ws,
    restHeartRate = fromIntegral . (!! 11) $ ws,
    intervalRestTime = Utils.parseDuration . Utils.grabChunk 12 2 $ ws,
    intervalRestDistance = Utils.parseBigEndian . Utils.grabChunk 14 2 $ ws
}
