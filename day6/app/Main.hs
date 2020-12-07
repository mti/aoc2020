module Main where

import Data.List

paragraphs :: String -> [[String]]
paragraphs = filter (/=[""]) . groupBy (\x y -> null x == null y) . lines

part1 :: String -> String
part1 = show . sum . map (length . nub . sort . concat) . paragraphs

part2 :: String -> String
part2 = show . sum . map (length . foldr1 intersect) . paragraphs

main :: IO ()
main = interact $ part2
