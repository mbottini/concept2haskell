{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.DistanceFrame where

import qualified Utils
import Data.Time.Clock
import Data.Word
import Data.Aeson

data DistanceFrame = DistanceFrame {
    duration :: DiffTime,
    heartRate :: Int,
    strokesPerMinute :: Int
} deriving(Show)

parseDistanceFrame :: [Word8] -> DistanceFrame
parseDistanceFrame ws = DistanceFrame {
    duration = Utils.parseDuration . Utils.grabChunk 0 2 $ ws,
    heartRate = fromIntegral . (!! 2) $ ws,
    strokesPerMinute = fromIntegral . (!! 3) $ ws
}

instance ToJSON DistanceFrame where
    toJSON df = object [ 
        "type" .= String "distance",
        "time" .= Number (Utils.tenthsToScientific . duration $ df),
        "stroke_rate" .= Number (Utils.intToScientific . strokesPerMinute $ df)]
-- TODO: Heartrate?
