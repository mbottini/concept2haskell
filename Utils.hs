module Utils where
import Data.Word

parseLittleEndian :: [Word8] -> Int
parseLittleEndian [] = 0
parseLittleEndian (x:xs) = (fromIntegral x) + 256 * (parseLittleEndian xs)

parseBigEndian :: [Word8] -> Int
parseBigEndian = parseLittleEndian . reverse

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

