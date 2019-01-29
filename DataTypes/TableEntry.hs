module DataTypes.TableEntry where

import qualified DataTypes.WorkoutType as Wt
import Data.Word
import qualified Utils

data TableEntry = TableEntry {
    workoutType :: Wt.WorkoutType,
    recordOffset :: Int,
    recordSize :: Int,
    index :: Int
} deriving(Show)

parseTableEntry :: [Word8] -> TableEntry
parseTableEntry lst = TableEntry {
    workoutType = Wt.parseWorkoutType . fromIntegral . (!! 1) $ lst,
    recordOffset = Utils.parseLittleEndian . (Utils.grabChunk 16 2) $ lst,
    recordSize = Utils.parseLittleEndian . (Utils.grabChunk 24 2) $ lst,
    index = Utils.parseLittleEndian . (Utils.grabChunk 26 2) $ lst
}
