import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word
import Data.Aeson.Encode.Pretty

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.Workout as W
import qualified Utils

getTableEntries :: [Char] -> IO [Te.TableEntry]
getTableEntries filename = Utils.getLogDataAccessBinaries filename >>=
                           Utils.inIO Utils.allButLast >>=
                           Utils.inIO (map Te.parseTableEntry) 

getWorkouts :: [Te.TableEntry] -> [Word8] -> [W.Workout]
getWorkouts tes ds = map (\te -> W.getFrames te ds) tes

main = do
    entries <- getTableEntries "LogDataAccessTbl.bin"
    workoutData <- Utils.getLogDataAccessData "LogDataStorage.bin"
    mapM W.toLocalTime (getWorkouts entries workoutData) >>=
        Utils.inIO Utils.listJSONToObj >>=
        Utils.inIO encodePretty >>=
        Utils.inIO BC.unpack >>=
        putStrLn
    
    
    
