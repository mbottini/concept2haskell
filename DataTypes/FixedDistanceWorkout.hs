{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module DataTypes.FixedDistanceWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedDistanceHeader as Fdh
import qualified DataTypes.DistanceFrame as Df
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word
import Data.Aeson
import Data.Aeson.Types

data FixedDistanceWorkout = FixedDistanceWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fdh.FixedDistanceHeader,
    frames :: [Df.DistanceFrame]
} deriving(Show)

parseFixedDistanceWorkout :: [Word8] -> [Word8] -> FixedDistanceWorkout
parseFixedDistanceWorkout hs ds = FixedDistanceWorkout {
    tableEntry = te,
    header = Fdh.parseFixedDistanceHeader chunk,
    frames = map Df.parseDistanceFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.fixedHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> [Word8]-> FixedDistanceWorkout
getFrames te bs = FixedDistanceWorkout {
    tableEntry = te,
    header = Fdh.parseFixedDistanceHeader chunk,
    frames = map Df.parseDistanceFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ bs
}
    where chunk = Utils.grabChunk offset Consts.fixedHeaderSize bs
          offset = Te.recordOffset te
          index = Te.recordSize te

instance ToJSON FixedDistanceWorkout where
    toJSON w = Utils.mergeObjects splits (toJSON (header w))
        where splits = (object ["workout" .= object ["splits" .= fs]])
              total = Fdh.totalDistance . header $ w
              t = Fdh.splitSize . header $ w
              fs = listValue id . 
                   Utils.addDistanceToIntervals t total .
                   map toJSON . 
                   frames $ w
