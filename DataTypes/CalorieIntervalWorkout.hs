module DataTypes.CalorieIntervalWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.CalorieIntervalHeader as Cih
import qualified DataTypes.CalorieIntervalFrame as Cif
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word
import Data.Aeson

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

