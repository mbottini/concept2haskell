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

getMetersFromDf :: Int -> DistanceFrame -> Int
getMetersFromDf cals dif = Utils.calsToMeters (duration dif) cals

-- We're performing an additional function here, so we can't abstract it
-- with addScientificToIntervals. We'd need to apply the identity function
-- with the first two cases, which is silly.

addCalDistanceToSplits :: Int -> Int -> [DistanceFrame] -> [Value]
addCalDistanceToSplits _ _ [] = error "Provided empty list!!"
addCalDistanceToSplits _ remaining (y:[]) =
    [Utils.mergeObjects (object ["distance" .= Number (Utils.intToScientific remaining)]) (toJSON y)]
addCalDistanceToSplits x remaining (y:ys) =
    resultObj : addCalDistanceToSplits x (remaining - ms) ys
        where ms = getMetersFromDf x y
              resultObj = Utils.mergeObjects (object ["distance" .= Number (Utils.intToScientific ms)]) (toJSON y)
