module DataTypes.TimeIntervalWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedIntervalHeader as Fih
import qualified DataTypes.TimeIntervalFrame as Tif
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word

data TimeIntervalWorkout = TimeIntervalWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fih.FixedIntervalHeader,
    frames :: [Tif.TimeIntervalFrame]
} deriving(Show)

parseTimeIntervalWorkout :: [Word8] -> [Word8] -> TimeIntervalWorkout
parseTimeIntervalWorkout hs ds = TimeIntervalWorkout {
    tableEntry = te,
    header = Fih.parseFixedIntervalHeader chunk,
    frames = map Tif.parseTimeIntervalFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> TimeIntervalWorkout
getFrames te = TimeIntervalWorkout {
    tableEntry = te,
    header = Fih.parseFixedIntervalHeader chunk,
    frames = map Tif.parseTimeIntervalFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.intervalHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where chunk = Utils.grabChunk offset Consts.intervalHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te
