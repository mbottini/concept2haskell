module DataTypes.FixedTimeWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedHeader as Fh
import qualified DataTypes.TimeFrame as Tf
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word

data FixedTimeWorkout = FixedTimeWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fh.FixedHeader,
    frames :: [Tf.TimeFrame]
} deriving(Show)

parseFixedTimeWorkout :: [Word8] -> [Word8] -> FixedTimeWorkout
parseFixedTimeWorkout hs ds = FixedTimeWorkout {
    tableEntry = te,
    header = Fh.parseFixedHeader chunk,
    frames = map Tf.parseTimeFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ ds
}
    where te = Te.parseTableEntry hs
          chunk = Utils.grabChunk offset Consts.fixedHeaderSize ds
          offset = Te.recordOffset te
          index = Te.recordSize te

getFrames :: Te.TableEntry -> [Word8]-> FixedTimeWorkout
getFrames te bs = FixedTimeWorkout {
    tableEntry = te,
    header = Fh.parseFixedHeader chunk,
    frames = map Tf.parseTimeFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ bs
}
    where chunk = Utils.grabChunk offset Consts.fixedHeaderSize bs
          offset = Te.recordOffset te
          index = Te.recordSize te
