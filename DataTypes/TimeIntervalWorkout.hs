{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.TimeIntervalWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.TimeIntervalHeader as Tih
import qualified DataTypes.TimeIntervalFrame as Tif
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word
import Data.Aeson
import Data.Aeson.Types

data TimeIntervalWorkout = TimeIntervalWorkout {
    tableEntry :: Te.TableEntry,
    header :: Tih.TimeIntervalHeader,
    frames :: [Tif.TimeIntervalFrame]
} deriving(Show)

parseTimeIntervalWorkout :: [Word8] -> [Word8] -> TimeIntervalWorkout
parseTimeIntervalWorkout hs ds = TimeIntervalWorkout {
    tableEntry = te,
    header = Tih.parseTimeIntervalHeader chunk,
    frames = map Tif.parseTimeIntervalFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> [Word8] -> TimeIntervalWorkout
getFrames te ds = TimeIntervalWorkout {
    tableEntry = te,
    header = Tih.parseTimeIntervalHeader chunk,
    frames = map Tif.parseTimeIntervalFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

instance ToJSON TimeIntervalWorkout where
    toJSON w = Utils.mergeObjects derivedValues (toJSON (header w))
        where derivedValues = (object ["workout" .= object ["intervals" .= fs],
                                       "stroke_rate" .= sr,
                                       "time" .= t])
              numIntervals = Tih.numSplits . header $ w
              sr = Number (Utils.intToScientific .
                           Utils.average . 
                           map Tif.strokesPerMinute .
                           frames $ w)
              t = Number (Utils.tenthsToScientific .
                           (Utils.multiplyInterval numIntervals) . 
                           Tih.splitSize .
                           header $ w)
              fs = populateFrames (header w) (frames w)

populateFrames :: Tih.TimeIntervalHeader -> 
                  [Tif.TimeIntervalFrame] ->
                  Value
populateFrames tih = 
    listValue id .
    map (Utils.addAttribute "rest_time" rt) .
    map (Utils.addAttribute "time" t) .
    map toJSON
        where rt = Number $ Utils.tenthsToScientific . Tih.restTime $ tih
              t = Number $ Utils.tenthsToScientific . Tih.splitSize $ tih
