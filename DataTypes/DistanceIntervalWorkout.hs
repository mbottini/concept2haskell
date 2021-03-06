{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.DistanceIntervalWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.DistanceIntervalHeader as Dih
import qualified DataTypes.DistanceIntervalFrame as Dif
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word
import Data.Aeson
import Data.Aeson.Types

data DistanceIntervalWorkout = DistanceIntervalWorkout {
    tableEntry :: Te.TableEntry,
    header :: Dih.DistanceIntervalHeader,
    frames :: [Dif.DistanceIntervalFrame]
} deriving(Show)

parseDistanceIntervalWorkout :: [Word8] -> [Word8] -> DistanceIntervalWorkout
parseDistanceIntervalWorkout hs ds = DistanceIntervalWorkout {
    tableEntry = te,
    header = Dih.parseDistanceIntervalHeader chunk,
    frames = map Dif.parseDistanceIntervalFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> [Word8] -> DistanceIntervalWorkout
getFrames te ds = DistanceIntervalWorkout {
    tableEntry = te,
    header = Dih.parseDistanceIntervalHeader chunk,
    frames = map Dif.parseDistanceIntervalFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

instance ToJSON DistanceIntervalWorkout where
    toJSON w = Utils.mergeObjects derivedValues (toJSON (header w))
        where derivedValues = (object ["workout" .= object ["intervals" .= fs],
                                       "stroke_rate" .= sr,
                                       "distance" .= dt])
              numIntervals = Dih.numSplits . header $ w
              sr = Number (Utils.intToScientific .
                           Utils.average . 
                           map Dif.strokesPerMinute .
                           frames $ w)
              dt = Number (Utils.intToScientific .
                           (* numIntervals) . 
                           Dih.splitSize .
                           header $ w)
              fs = populateFrames (header w) (frames w)

populateFrames :: Dih.DistanceIntervalHeader -> 
                  [Dif.DistanceIntervalFrame] ->
                  Value
populateFrames dih = 
    listValue id .
    map (Utils.addAttribute "rest_time" rt) .
    map (Utils.addAttribute "distance" dt) .
    map toJSON
        where rt = Number $ Utils.tenthsToScientific . Dih.restTime $ dih
              dt = Number $ Utils.intToScientific . Dih.splitSize $ dih
