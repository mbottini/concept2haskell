module Utils where
import Data.Word
import Data.Time
import Data.Scientific
import Data.Aeson
import qualified Data.HashMap.Lazy as HML

parseLittleEndian :: [Word8] -> Int
parseLittleEndian [] = 0
parseLittleEndian (x:xs) = (fromIntegral x) + 256 * (parseLittleEndian xs)

parseBigEndian :: [Word8] -> Int
parseBigEndian = parseLittleEndian . reverse

type Bit = Bool

wordToBits :: Word8 -> [Bit]
wordToBits = wordToBits' 0

wordToBits' :: Int -> Word8 -> [Bit]
wordToBits' 8 w = []
wordToBits' n w
    | odd w = True : wordToBits' (n+1) (div w 2)
    | otherwise = False : wordToBits' (n+1) (div w 2)

wordsToBits :: [Word8] -> [Bit]
wordsToBits xs = xs >>= wordToBits

wordsToBitsReverse :: [Word8] -> [Bit]
wordsToBitsReverse xs = xs >>= (reverse . wordToBits)

bitsToInt :: [Bit] -> Int
bitsToInt [] = 0
bitsToInt (True:xs) = 1 + 2 * (bitsToInt xs)
bitsToInt (False:xs) = 2 * (bitsToInt xs)

grabChunk :: Int -> Int -> [a] -> [a]
grabChunk 0 0 _  = []
grabChunk _ _ [] = []
grabChunk 0 amount (x:xs) = x : grabChunk 0 (amount-1) xs
grabChunk offset amount (x:xs) = grabChunk (offset-1) amount xs

splitAll :: Int -> [a] -> [[a]]
splitAll _ [] = []
splitAll chunkSize xs = xxs : (splitAll chunkSize ys)
    where (xxs, ys) = splitAt chunkSize xs

allButLast :: [a] -> [a]
allButLast [] = []
allButLast (x:[]) = []
allButLast (x:xs) = x : allButLast xs

createUTCTime :: Int -> Int -> Int -> Int -> Int -> UTCTime
createUTCTime year month day hours minutes = UTCTime date diff
    where date = fromGregorian (fromIntegral year) month day
          diff = secondsToDiffTime . fromIntegral $ (60*minutes + 3600*hours)

parseDateStamp :: [Word8] -> UTCTime
parseDateStamp xs = createUTCTime year month day hours minutes
    where bits = wordsToBitsReverse xs
          year = parseYear . grabChunk 0 7 $ bits
          day = bitsToInt . reverse . grabChunk 7 5 $ bits
          month = bitsToInt . reverse . grabChunk 12 4 $ bits
          hours = bitsToInt . reverse . grabChunk 16 8 $ bits
          minutes = bitsToInt . reverse . grabChunk 24 8 $ bits

parseYear :: [Bit] -> Int
parseYear bs = 2000 + (bitsToInt . reverse $ bs)

parseDuration :: [Word8] -> DiffTime
parseDuration = picosecondsToDiffTime . 
                (*100000000000) .
                toInteger . 
                parseBigEndian

parseSecs :: [Word8] -> DiffTime
parseSecs = secondsToDiffTime .
            toInteger .
            parseBigEndian

tenthsToScientific :: DiffTime -> Scientific
tenthsToScientific = unsafeFromRational . (*10) . toRational

intToScientific :: Int -> Scientific
intToScientific = unsafeFromRational . fromIntegral

mergeObjects :: Value -> Value -> Value
mergeObjects (Object x) (Object y) = Object $ HML.union x y
mergeObjects _ _ = error "Can't merge non-object values!"

inIO :: Monad m => (a -> b) -> a -> m b
inIO f = return . f
