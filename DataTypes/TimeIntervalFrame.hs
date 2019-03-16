{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.TimeIntervalFrame where

import Data.Word
import Data.Aeson
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

instance ToJSON TimeIntervalFrame where
    toJSON tif = object [
        "type" .= String "time",
        "distance" .= Number (Utils.intToScientific . distance $ tif),
        "stroke_rate" .= Number (Utils.intToScientific . strokesPerMinute $ tif)]
