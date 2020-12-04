module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Data.Char
import Data.Either
import Data.List
import qualified Data.Map.Strict as M
import Text.Parsec.Combinator (many1, sepBy, endBy, sepEndBy)
import Control.Monad
import Control.Arrow


type Key = String
type Value = String
type Passport = M.Map Key Value

-- emptyLine :: Parser ()
-- emptyLine = newline <* newline

passportList :: Parser [Passport]
passportList = passport `sepEndBy` newline

passport :: Parser Passport
passport = liftM M.fromList (keyValue `endBy` oneOf " \n\r")

keyValue :: Parser (Key,Value)
keyValue = do
    key <- many1 (satisfy (`notElem` ": \n\r"))
    void $ char ':'
    val <- many1 (satisfy (`notElem` ": \n\r"))
    return (key,val)

parsePassports :: String -> [Passport]
parsePassports s = fromRight [] $ parse passportList "" s

showPassport :: Passport -> String
showPassport p = "Passport fields: " ++ (intercalate " " $ M.keys p) ++ " (" ++ show (isValid p) ++ ")"

isValid :: Passport -> Bool
isValid p = all (`M.member` p) mandatory where
  mandatory = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isCorrect :: Passport -> Bool
isCorrect p = and
    [inRange 1920 2002 $ read (p M.! "byr"),
     inRange 2010 2020 $ read (p M.! "iyr"),
     inRange 2020 2030 $ read (p M.! "eyr"),
     validHgt (p M.! "hgt"),
     validHcl (p M.! "hcl"),
     elem (p M.! "ecl") ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
     validPid (p M.! "pid")]

validHgt :: String -> Bool
validHgt p = let l = length p in
    if l < 2 then False else case ((read *** id) $ splitAt (l-2) p) of
        (x, "cm") -> inRange 150 193 x
        (y, "in") -> inRange 59 76 y
        otherwise -> False

validHcl :: String -> Bool
validHcl ('#':s) = (length s == 6) && all (`elem` "0123456789abcdef") s
validHcl _       = False

validPid :: String -> Bool
validPid s = (length s == 9) && all (`elem` "0123456789") s

inRange :: Integer -> Integer -> Integer -> Bool
inRange a b x = a <= x && x <= b

part1 :: String -> String
part1 = show . length . filter isValid . parsePassports

part2 :: String -> String
part2 = show . length . filter isCorrect . filter isValid . parsePassports

main :: IO ()
main = interact $ part2

{-
 - vim: ts=4 expandtab
 -}
