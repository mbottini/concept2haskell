module DataTypes.FixedDistanceWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedHeader as Fh
import qualified DataTypes.DistanceFrame as Df
import qualified Utils
import qualified DataTypes.Consts as Consts
import Data.Word

data FixedDistanceWorkout = FixedDistanceWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fh.FixedHeader,
    frames :: [Df.DistanceFrame]
} deriving(Show)

parseFixedDistanceWorkout :: [Word8] -> [Word8] -> FixedDistanceWorkout
parseFixedDistanceWorkout hs ds = FixedDistanceWorkout {
    tableEntry = te,
    header = Fh.parseFixedHeader chunk,
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
    header = Fh.parseFixedHeader chunk,
    frames = map Df.parseDistanceFrame . 
             Utils.splitAll Consts.frameSize .
             drop Consts.fixedHeaderSize .
             Utils.grabChunk offset index $ bs
}
    where chunk = Utils.grabChunk offset Consts.fixedHeaderSize bs
          offset = Te.recordOffset te
          index = Te.recordSize te
