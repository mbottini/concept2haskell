import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Binary.Get
import Data.Time.Clock

import qualified DataTypes.WorkoutType as Wt
import qualified DataTypes.FixedHeader as Fh
import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedIntervalHeader as Fih
import qualified DataTypes.VariableIntervalHeader as Vih
import qualified DataTypes.DistanceFrame as Df
import qualified DataTypes.TimeFrame as Tf
import qualified DataTypes.DistanceIntervalFrame as Dif
import qualified DataTypes.TimeIntervalFrame as Tif
import qualified DataTypes.VariableIntervalFrame as Vif
import qualified DataTypes.FixedDistanceWorkout as Fdw
import qualified DataTypes.FixedTimeWorkout as Ftw
import qualified DataTypes.VariableIntervalWorkout as Viw
import qualified Utils


-- Takes a bytestream and returns a Get containing a list of bytes.
gBytes :: Get [Word8]
gBytes = do
  e <- isEmpty
  case e of
    True -> return []
    False -> do
      current <- getWord8
      rest <- gBytes
      return (current : rest)

-- Opens a binary file and returns an IO containing the bytes in the list.
getLogDataAccessData :: [Char] -> IO [Word8]
getLogDataAccessData filename = do
    bStream <- B.readFile filename
    return $ runGet gBytes bStream

getLogDataAccessBinaries :: [Char] -> IO [[Word8]]
getLogDataAccessBinaries filename = do
    data_lst <- getLogDataAccessData filename
    return $ Utils.splitAll 32 data_lst

getTableEntries :: [Char] -> IO [Te.TableEntry]
getTableEntries filename = getLogDataAccessBinaries filename >>=
                           Utils.inIO Utils.allButLast >>=
                           Utils.inIO (map Te.parseTableEntry) 

main = do
    entries <- getTableEntries "LogDataAccessTbl.bin"
    workoutData <- getLogDataAccessData "LogDataStorage.bin"
    putStrLn . show $ entries
