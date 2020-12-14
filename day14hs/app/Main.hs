module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Data.Char
import Data.Either
import Data.List
import Text.Parsec.Combinator (many1, sepBy, endBy, sepEndBy)
import qualified Data.Map.Strict as M
import Data.Bits
import Numeric
import Control.Monad

type Mask = String
type Addr = Int
type Val  = Integer
data Inst = SetMask Mask | SetMem Addr Val deriving Show
type Mem  = M.Map Addr Val

parseProgram :: String -> [Inst]
parseProgram = fromRight [] . parse program ""

program :: Parser [Inst]
program = (try mask <|> mem) `sepEndBy` newline

mask :: Parser Inst
mask = do
    void $ string "mask = "
    mask <- many1 (oneOf "01X")
    return $ SetMask mask

mem :: Parser Inst
mem = do
    void $ string "mem["
    addr <- many1 digit
    void $ string "] = "
    val <- many1 digit
    return $ SetMem (read addr) (read val)

readBits :: (Integral a, Read a) => String -> a
readBits = foldl (\x y -> 2*x + y) 0 . map (\c -> read [c])

subsX :: Char -> String -> String
subsX c = map (\x -> if x=='X' then c else x)

paddedBin :: (Integral a, Show a) => Int -> a -> String
paddedBin s n = (replicate (s - (length bits)) '0') ++ bits where
  bits = showIntAtBase 2 intToDigit n ""

runProgV1 :: [Inst] -> Mem
runProgV1 = go M.empty 0 (-1) where
    go m _ _ [] = m
    go m _ _ ((SetMask mask):xs) =
        go m (readBits $ subsX '1' mask) (readBits $ subsX '0' mask) xs
    go m andMask orMask ((SetMem addr val):xs) =
        go (M.insert addr ((val .|. orMask) .&. andMask) m) andMask orMask xs 

runProgV2 :: [Inst] -> Mem
runProgV2 = go M.empty (replicate 36 '0') where
    go m _ [] = m
    go m _ ((SetMask mask):xs) = go m mask xs
    go m mask ((SetMem addr val):xs) =
        let baseaddr = paddedBin 36 addr
            maskaddr = map (\(x,y) -> if y=='0' then x else y) $ zip baseaddr mask
            addrs = map readBits . sequence $
                    map (\x -> if x=='X' then "01" else [x]) maskaddr
        in go (foldr (\a m' -> M.insert a val m') m addrs) mask xs 

part1 :: String -> String
part1 = show . sum . M.elems . runProgV1 . parseProgram

part2 :: String -> String
part2 = show . sum . M.elems . runProgV2 . parseProgram

main :: IO ()
main = interact $ part2

{-
 - vim: ts=4 expandtab
 -}
