{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.VariableIntervalWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.VariableIntervalHeader as Vih
import qualified DataTypes.VariableIntervalFrame as Vif
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word
import Data.Aeson
import Data.Aeson.Types

data VariableIntervalWorkout = VariableIntervalWorkout {
    tableEntry :: Te.TableEntry,
    header :: Vih.VariableIntervalHeader,
    frames :: [Vif.VariableIntervalFrame]
} deriving(Show)

parseVariableIntervalWorkout :: [Word8] -> [Word8] -> VariableIntervalWorkout
parseVariableIntervalWorkout hs ds = VariableIntervalWorkout {
    tableEntry = te,
    header = Vih.parseVariableIntervalHeader chunk,
    frames = map Vif.parseVariableIntervalFrame . 
             Utils.splitAll Consts.variableIntervalFrameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> [Word8] -> VariableIntervalWorkout
getFrames te ds = VariableIntervalWorkout {
    tableEntry = te,
    header = Vih.parseVariableIntervalHeader chunk,
    frames = map Vif.parseVariableIntervalFrame . 
             Utils.splitAll Consts.variableIntervalFrameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

instance ToJSON VariableIntervalWorkout where
    toJSON w = Utils.mergeObjects splits (toJSON (header w))
        where splits = (object ["workout" .= object ["intervals" .= fs],
                                "stroke_rate" .= Number
                                    (Utils.intToScientific .
                                    Utils.averageWeighted .
                                    map Vif.getRestTimeSPM .
                                    frames $ w),
                                "rest_distance" .= Number
                                    (Utils.intToScientific .
                                    sum .
                                    map Vif.intervalRestDistance .
                                    frames $ w),
                                "rest_time" .= Number
                                    (Utils.tenthsToScientific .
                                    sum .
                                    map Vif.intervalRestTime .
                                    frames $ w)])
              fs = listValue id . 
                   map toJSON . 
                   frames $ w