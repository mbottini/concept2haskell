{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.CalorieIntervalWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.CalorieIntervalHeader as Cih
import qualified DataTypes.CalorieIntervalFrame as Cif
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word
import Data.Aeson
import Data.Aeson.Types

data CalorieIntervalWorkout = CalorieIntervalWorkout {
    tableEntry :: Te.TableEntry,
    header :: Cih.CalorieIntervalHeader,
    frames :: [Cif.CalorieIntervalFrame]
} deriving(Show)

parseCalorieIntervalWorkout :: [Word8] -> [Word8] -> CalorieIntervalWorkout
parseCalorieIntervalWorkout hs ds = CalorieIntervalWorkout {
    tableEntry = te,
    header = Cih.parseCalorieIntervalHeader chunk,
    frames = map Cif.parseCalorieIntervalFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> [Word8] -> CalorieIntervalWorkout
getFrames te ds = CalorieIntervalWorkout {
    tableEntry = te,
    header = Cih.parseCalorieIntervalHeader chunk,
    frames = map Cif.parseCalorieIntervalFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

instance ToJSON CalorieIntervalWorkout where
    toJSON w = Utils.mergeObjects derivedValues (toJSON (header w))
        where derivedValues = (object ["workout" .= object ["intervals" .= fs],
                                       "stroke_rate" .= sr,
                                       "distance" .= dt])
              sr = Number (Utils.intToScientific .
                           Utils.average . 
                           map Cif.strokesPerMinute .
                           frames $ w)
              fs = populateFrames (header w) (frames w)
              dt = Number (Utils.intToScientific .
                           sum .
                           map (Cif.getMetersFromCif 
                               (Cih.splitSize . header $ w)) .
                           frames $ w)
                           

populateFrames :: Cih.CalorieIntervalHeader -> 
                  [Cif.CalorieIntervalFrame] ->
                  Value
populateFrames cih = 
    listValue id .
    map (Utils.addAttribute "rest_time" rt) .
    map (Cif.insertDistance (Cih.splitSize cih))
        where rt = Number $ Utils.tenthsToScientific . Cih.restTime $ cih
