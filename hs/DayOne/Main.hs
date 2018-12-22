module Main where

main :: IO ()
main = do
  putStrLn "Advent of Code Day 1.1"
  contents <- getContents
  print $ adjustFrequency contents

read' :: String -> Int
read' s
  | head s == '+' = read $ tail s
  | otherwise = read s

adjustFrequency :: String -> Int
adjustFrequency contents = foldl (\acc x -> acc + read' x) 0 (lines contents)
