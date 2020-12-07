module Main where

import Data.List

seatID :: String -> Int
seatID = foldr (\x y -> x + 2*y) 0 . reverse .
            map (\x -> if (x `elem` "FL") then 0 else 1)

part2 :: String -> String
part2 = show . findMissing . sort . map seatID . lines
    where
        findMissing  [] = -1
        findMissing [x] = x+1
        findMissing (x:y:ys) = if y==x+2 then x+1 else findMissing (y:ys) 

part1 :: String -> String
part1 = show . maximum . map seatID . lines

main :: IO ()
main = interact $ part2

{-
 - vim: ts=4 expandtab
 -}
