module DataTypes.FixedTimeWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedHeader as Fh
import qualified DataTypes.TimeFrame as Tf
import qualified Utils

import Data.Word

data FixedTimeWorkout = FixedTimeWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fh.FixedHeader,
    frames :: [Tf.TimeFrame]
} deriving(Show)
 
parseFixedTimeWorkout :: [Word8] -> [Word8] -> FixedTimeWorkout
parseFixedTimeWorkout teLst rawFrameLst = FixedTimeWorkout {
    tableEntry = newEntry,
    header = newHeader,
    frames = newFrames
}
    where newEntry = Te.parseTableEntry teLst
          rawFrameChunk = Utils.grabChunk (Te.recordOffset newEntry) (Te.recordSize newEntry) rawFrameLst
          newHeader = Fh.parseFixedHeader (Utils.grabChunk 0 52 rawFrameChunk)
          newFrames = map Tf.parseTimeFrame (Utils.splitAll 52 . drop 52 $ rawFrameChunk)

