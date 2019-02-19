module DataTypes.DistanceIntervalWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.DistanceIntervalHeader as Dih
import qualified DataTypes.DistanceIntervalFrame as Dif
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word

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
