module Main where

import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "Advent of Code Day 2.1"
  contents <- getContents
  print $ "The checksum is: " ++ (show $ calculateChecksum contents)
  putStrLn "Advent of Code Day 2.2"

countLetter :: Char -> Map.Map Char Int -> Map.Map Char Int
countLetter c counts = Map.insertWith (+) c 1 counts

countLetters :: String -> Map.Map Char Int
countLetters line = foldr countLetter Map.empty line

sumCounts :: Map.Map Char Int -> Map.Map Int Int
sumCounts counts = Map.foldr (\v sums -> Map.insertWith (+) v 1 sums) Map.empty
  $ Map.filter (\v -> v == 2 || v == 3) counts

collectSums :: [String] -> [Map.Map Int Int]
collectSums ss = filter (not . Map.null) $
  map (\line -> sumCounts $ countLetters line) ss

countUp :: Map.Map Int Int -> Map.Map Int Int -> Map.Map Int Int
countUp counts totalCounts = Map.foldrWithKey (\k v acc -> Map.insertWith (+) k 1 acc) totalCounts counts

checksum :: Map.Map Int Int -> Int
checksum counts = Map.foldr (\v acc -> v * acc) 1 counts

calculateChecksum :: String -> Int
calculateChecksum contents = checksum $
  foldr (\counts acc -> countUp counts acc) Map.empty $ collectSums (lines contents)
