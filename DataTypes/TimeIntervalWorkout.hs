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
    toJSON w = Utils.mergeObjects splits (toJSON (header w))
        where splits = (object ["workout" .= object ["intervals" .= fs]])
              numIntervals = Tih.numSplits . header $ w
              t = Tih.splitSize . header $ w
              total = Utils.multiplyInterval numIntervals t
              fs = listValue id . 
                   Utils.addTimeToIntervals t total .
                   map toJSON . 
                   frames $ w
