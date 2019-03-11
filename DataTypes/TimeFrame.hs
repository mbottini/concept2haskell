{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.TimeFrame where

import Data.Word
import qualified Utils
import Data.Aeson

data TimeFrame = TimeFrame {
    distance :: Int,
    heartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseTimeFrame :: [Word8] -> TimeFrame
parseTimeFrame ws = TimeFrame {
    distance = Utils.parseBigEndian . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    strokesPerMinute = fromIntegral . (!! 3) $ ws
}

instance ToJSON TimeFrame where
    toJSON df = object [
        "type" .= String "time",
        "distance" .= Number (Utils.intToScientific . distance $ df),
        "stroke_rate" .= Number (Utils.intToScientific . strokesPerMinute $ df)]
