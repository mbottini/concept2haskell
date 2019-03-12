import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word
import Data.Binary.Get
import Data.Time.Clock
import Data.Aeson
import Data.Aeson.Encode.Pretty

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.Workout as W
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

getWorkouts :: [Te.TableEntry] -> [Word8] -> [W.Workout]
getWorkouts tes ds = map (\te -> W.getFrames te ds) tes

main = do
    entries <- getTableEntries "LogDataAccessTbl.bin"
    workoutData <- getLogDataAccessData "LogDataStorage.bin"
    mapM W.toLocalTime (getWorkouts entries workoutData) >>=
        Utils.inIO Utils.listJSONToObj >>=
        Utils.inIO encodePretty >>=
        Utils.inIO BC.unpack >>=
        putStrLn
    
    
    
