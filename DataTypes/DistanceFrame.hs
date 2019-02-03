module DataTypes.DistanceFrame where

import qualified Utils
import Data.Word

data DistanceFrame = DistanceFrame {
    distance :: Int,
    heartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseDistanceFrame :: [Word8] -> DistanceFrame
parseDistanceFrame ws = DistanceFrame {
    distance = Utils.parseBigEndian . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    strokesPerMinute = fromIntegral . (!! 3) $ ws
}
