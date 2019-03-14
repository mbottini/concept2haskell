{-# LANGUAGE OverloadedStrings #-} -- Needed for JSON string assignments

module Utils where
import Data.Word
import Data.Time
import Data.Scientific
import Data.Aeson
import qualified Data.Vector
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as DT
import Data.Time.LocalTime

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

{-
createUTCTime :: Int -> Int -> Int -> Int -> Int -> UTCTime
createUTCTime year month day hours minutes = UTCTime date diff
    where date = fromGregorian (fromIntegral year) month day
          diff = secondsToDiffTime . fromIntegral $ (60*minutes + 3600*hours)
-}

createLocalTime :: Int -> Int -> Int -> Int -> Int -> LocalTime
createLocalTime year month day hours minutes = LocalTime date time
    where date = fromGregorian (fromIntegral year) month day
          time = TimeOfDay hours minutes 0

parseDateStamp :: [Word8] -> LocalTime
parseDateStamp xs = createLocalTime year month day hours minutes
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

multiplyInterval :: Int -> DiffTime -> DiffTime
multiplyInterval n = secondsToDiffTime . 
                     floor . 
                     (* (fromIntegral n)) . 
                     toRational

secsToScientific :: DiffTime -> Scientific
secsToScientific = unsafeFromRational . toRational

tenthsToScientific :: DiffTime -> Scientific
tenthsToScientific = (*10) . secsToScientific

intToScientific :: Int -> Scientific
intToScientific = unsafeFromRational . fromIntegral

mergeObjects :: Value -> Value -> Value
mergeObjects (Object x) (Object y) = Object $ HML.union x y
mergeObjects _ _ = error "Can't merge non-object values!"

addAttribute :: String -> Value -> Value -> Value
addAttribute s val (Object x) = Object $ HML.insert (DT.pack s) val x
addAttribute _ _ _ = error "Can only add attribute to object!"

-- Concept2 allows the user to have "partial" splits. Say that I row for eleven
-- minutes. I can set the split to 2 minutes, which means that the final split
-- will be for 1 minute. This isn't tracked inside the split frame! Instead,
-- you have to take the total time, subtract from the total as you go through
-- the splits, and then give the remainder to the final partial split.
--
-- I wish I could abstract these two into a single function, but DiffTimes
-- aren't *quite* the same as Ints.

addScientificToIntervals :: Num a => String -> 
                                     (a -> Scientific) -> 
                                     a ->
                                     a ->
                                     [Value] ->
                                     [Value]
addScientificToIntervals _ _ _ _ [] = error "Provided empty list!!"
addScientificToIntervals s f x remaining (y:[]) = 
    [mergeObjects (object [DT.pack s .= Number (f remaining)]) y]
addScientificToIntervals s f x remaining (y:ys) =
    resultObj : addScientificToIntervals s f x (remaining - x) ys
        where resultObj = mergeObjects (object [DT.pack s .= Number (f x)]) y

addDistanceToIntervals :: Int -> Int -> [Value] -> [Value]
addDistanceToIntervals = addScientificToIntervals "distance" intToScientific

addTimeToIntervals :: DiffTime -> DiffTime -> [Value] -> [Value]
addTimeToIntervals = addScientificToIntervals "time" tenthsToScientific

-- Anu Dudhia has the following formula for the Concept2's calorie conversion
-- on a page of his personal website at
-- http://eodg.atm.ox.ac.uk/user/dudhia/rowing/physics/ergometer.html#section11
-- 
-- E = ( 4 W + 0.35 t ) / 4.2
-- where E is the number of calories, and W is work in kJ.
-- Since we have calories and duration, we need to solve for work.
-- Some simple algebra gets us
--
-- W = 7000/80 * (12E - t)
-- Brief check: More calories in the same duration = more work.
--              More time, same calories = less work.
--              I'm happy with this.

convertToJoules :: Rational -> Rational -> Rational
convertToJoules secs cals = 7000/80 * (12*cals - secs)

-- 
-- Since we have work and duration, we can get watts by dividing them.
-- 

convertToWatts :: Rational -> Rational -> Rational
convertToWatts secs joules = joules / secs

-- machars.net provides a formula to convert watts to meters per second as
-- follows:
--
-- v = (P / 2.8)^(1/3)
-- where v is in meters per second, and P is watts.

convertToVelocity :: Floating a => Rational -> a
convertToVelocity watts = ((fromRational watts) / 2.8)**(1/3)

-- Putting them together...

calsToMeters :: DiffTime -> Int -> Int
calsToMeters dt = floor .
                  (* (fromRational secs)) .
                  convertToVelocity . 
                  convertToWatts secs .
                  convertToJoules secs .
                  fromIntegral
    where secs = toRational dt

average :: [Int] -> Int
average xs = (sum xs) `div` (length xs)

averageWeighted :: [(DiffTime, Int)] -> Int
averageWeighted xs = floor $ (fromIntegral allStrokes) / ((toRational totalTime) / 60)
    where allStrokes = sum (map (uncurry totalStrokes) xs)
          totalTime = sum (map fst xs)

totalStrokes :: DiffTime -> Int -> Int
totalStrokes t spm = floor $ toRational t * (fromIntegral spm) / 60

listJSONToObj :: [Value] -> Value
listJSONToObj xs = Array (Data.Vector.fromList xs)

inIO :: Monad m => (a -> b) -> a -> m b
inIO f = return . f

-- Needs to be IO because the timezone is whatever is local to the computer.
formatTimeStamp :: LocalTime -> [Char]
formatTimeStamp = formatTime defaultTimeLocale "%F %T"

-- 2019-02-25 13:12:00 UTC

transformUTCStamp :: TimeZone -> Value -> Value
transformUTCStamp tz (Object x) = 
    Object (HML.insert "date" (String . DT.pack $(formatTimeStamp stamp)) x)
    where stamp = case (HML.lookup "date" x) of
            (Just (String s)) -> parseTimeOrError True defaultTimeLocale "%F %T" (DT.unpack s)
            Nothing -> error "Object does not contain timestamp!"
transformUTCStamp tz _ = error "Non-object value passed to function!"

transformUTCStampLocal :: Value -> IO Value
transformUTCStampLocal v = do
    tz <- getCurrentTimeZone
    return (transformUTCStamp tz v)
    
