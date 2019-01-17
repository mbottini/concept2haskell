import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Binary.Get

data WorkoutType = FreeRow |
                   SingleDistance |
                   SingleTime |
                   TimedInterval |
                   DistanceInterval |
                   VariableInterval |
                   SingleCalorie
                   deriving(Show)

parseWorkoutType :: Int -> WorkoutType
parseWorkoutType 1 = FreeRow
parseWorkoutType 3 = SingleDistance
parseWorkoutType 5 = SingleTime
parseWorkoutType 6 = TimedInterval
parseWorkoutType 7 = DistanceInterval
parseWorkoutType 8 = VariableInterval
parseWorkoutType 10 = SingleCalorie
parseWorkoutType n = error ("parseWorkoutType: Invalid number " ++ (show n))

data TableEntry = TableEntry {
    workoutType :: WorkoutType,
    recordOffset :: Int,
    recordSize :: Int,
    index :: Int
} deriving(Show)

parseTableEntry :: [Word8] -> TableEntry
parseTableEntry lst = TableEntry {
    workoutType = parseWorkoutType . fromIntegral . (!! 1) $ lst,
    recordOffset = parseLittleEndian . (grabChunk 16 2) $ lst,
    recordSize = parseLittleEndian . (grabChunk 24 2) $ lst,
    index = parseLittleEndian . (grabChunk 26 2) $ lst
}

parseLittleEndian :: [Word8] -> Int
parseLittleEndian [] = 0
parseLittleEndian (x:xs) = (fromIntegral x) + 256 * (parseLittleEndian xs)
 
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

-- "Slicing" - given an offset and an amount, return a sublist starting
-- at the offset's index and containing the amount of elements.
grabChunk :: Int -> Int -> [a] -> [a]
grabChunk 0 0 _  = []
grabChunk _ _ [] = []
grabChunk 0 amount (x:xs) = x : grabChunk 0 (amount-1) xs
grabChunk offset amount (x:xs) = grabChunk (offset-1) amount xs

-- Splits a list into a list of sublists of length chunkSize.
splitAll :: Int -> [a] -> [[a]]
splitAll _ [] = []
splitAll chunkSize xs = xxs : (splitAll chunkSize ys)
    where (xxs, ys) = splitAt chunkSize xs

-- Opens a binary file and returns an IO containing the bytes in the list.
getLogDataAccessData :: [Char] -> IO [Word8]
getLogDataAccessData filename = do
    bStream <- B.readFile filename
    return $ runGet gBytes bStream

getLogDataAccessBinaries :: [Char] -> IO [[Word8]]
getLogDataAccessBinaries filename = do
    data_lst <- getLogDataAccessData filename
    return $ splitAll 32 data_lst

allButLast :: [a] -> [a]
allButLast [] = []
allButLast (x:[]) = []
allButLast (x:xs) = x : allButLast xs

main = do
    accessEntries <- getLogDataAccessBinaries "LogDataAccessTbl.bin"
    putStrLn . show . (map parseTableEntry) . allButLast $ accessEntries
