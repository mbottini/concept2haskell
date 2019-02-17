module DataTypes.VariableIntervalWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.VariableIntervalHeader as Vih
import qualified DataTypes.VariableIntervalFrame as Vif
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word

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
