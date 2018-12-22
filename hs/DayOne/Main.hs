module Main where

import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn "Advent of Code Day 1.1"
  contents <- getContents
  print $ "The adjusted frequency is: " ++ show (adjustFrequency contents)
  putStrLn "Advent of Code Day 1.2"
  print $ "The repeated frequency is: " ++ show
    (findRepeatedFrequency (0, Set.singleton 0) $ (cycle . lines) contents)


read' :: String -> Int
read' s
  | head s == '+' = read $ tail s
  | otherwise = read s

addString :: String -> Int -> Int
addString s i =  read' s + i

adjustFrequency :: String -> Int
adjustFrequency contents = foldr addString 0 (lines contents)

addFrequency :: String -> Int -> Set.Set Int -> (Int, Set.Set Int)
addFrequency adjustment frequency frequencies =
  (frequency', Set.insert frequency' frequencies)
  where frequency' = addString adjustment frequency

checkFrequency :: Int -> Set.Set Int -> Bool
checkFrequency frequency frequencies = Set.member frequency frequencies

findRepeatedFrequency :: (Int, Set.Set Int) -> [String] -> Int
findRepeatedFrequency _ [] = error "No frequencies given to check"
findRepeatedFrequency (frequency, frequencies) (s:ss)
  | checkFrequency frequency' frequencies = frequency'
  | otherwise = findRepeatedFrequency (frequency', frequencies') ss
  where (frequency', frequencies') = addFrequency s frequency frequencies
