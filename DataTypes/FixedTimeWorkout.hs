{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.FixedTimeWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedTimeHeader as Fth
import qualified DataTypes.TimeFrame as Tf
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word
import Data.Aeson
import Data.Aeson.Types

data FixedTimeWorkout = FixedTimeWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fth.FixedTimeHeader,
    frames :: [Tf.TimeFrame]
} deriving(Show)

parseFixedTimeWorkout :: [Word8] -> [Word8] -> FixedTimeWorkout
parseFixedTimeWorkout hs ds = FixedTimeWorkout {
    tableEntry = te,
    header = Fth.parseFixedTimeHeader chunk,
    frames = map Tf.parseTimeFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.fixedHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> [Word8]-> FixedTimeWorkout
getFrames te bs = FixedTimeWorkout {
    tableEntry = te,
    header = Fth.parseFixedTimeHeader chunk,
    frames = map Tf.parseTimeFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ bs
}
    where chunk = Utils.grabChunk offset Consts.fixedHeaderSize bs
          offset = Te.recordOffset te
          index = Te.recordSize te

instance ToJSON FixedTimeWorkout where
    toJSON w = Utils.mergeObjects splits (toJSON (header w))
        where splits = (object ["workout" .= object ["splits" .= fs]])
              total = Fth.totalDuration . header $ w
              t = Fth.splitSize . header $ w
              fs = listValue id .
                   Utils.addTimeToIntervals t total .
                   map toJSON .
                   frames $ w
