{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.FixedCalorieWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedCalorieHeader as Fch
import qualified DataTypes.DistanceFrame as Df
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word
import Data.Aeson
import Data.Aeson.Types

data FixedCalorieWorkout = FixedCalorieWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fch.FixedCalorieHeader,
    frames :: [Df.DistanceFrame]
} deriving(Show)

parseFixedCalorieWorkout :: [Word8] -> [Word8] -> FixedCalorieWorkout
parseFixedCalorieWorkout hs ds = FixedCalorieWorkout {
    tableEntry = te,
    header = Fch.parseFixedCalorieHeader chunk,
    frames = map Df.parseDistanceFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.fixedHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> [Word8]-> FixedCalorieWorkout
getFrames te bs = FixedCalorieWorkout {
    tableEntry = te,
    header = Fch.parseFixedCalorieHeader chunk,
    frames = map Df.parseDistanceFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ bs
}
    where chunk = Utils.grabChunk offset Consts.fixedHeaderSize bs
          offset = Te.recordOffset te
          index = Te.recordSize te

instance ToJSON FixedCalorieWorkout where
    toJSON w = Utils.mergeObjects splits (toJSON (header w))
        where splits = (object ["workout" .= object ["splits" .= fs]])
              t = Fch.splitSize . header $ w
              total = Fch.totalDistance . header $ w
              fs = listValue id . 
                   Df.addCalDistanceToSplits t total .
                   frames $ w