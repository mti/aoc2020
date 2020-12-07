module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Data.Char
import Data.Either
import Data.List
import Data.Tuple
import Text.Parsec.Combinator (many1, sepBy, endBy, sepEndBy)
import Control.Monad
import Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Tree as T
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (reachable)


type Bag = String
type BagList = [(Int,Bag)]
type Rule = (Bag, BagList)

ruleList :: Parser [Rule]
ruleList = rule `sepEndBy` newline

rule :: Parser Rule
rule = do
    outBag <- bag
    void $ string "contain "
    list <- bagList
    return (outBag, list)

bag :: Parser Bag
bag = do
    adj1 <- many1 letter
    void $ space
    adj2 <- many1 letter
    void $ string " bag"
    skipMany (oneOf " s")
    return (adj1 ++ "-" ++ adj2)

bagList :: Parser BagList
bagList = (noBags <|> (someBag `sepBy1` string ", ")) <* char '.' where
    noBags = (void $ string "no other bags") >> pure []
    someBag = do
        num <- many1 (satisfy isDigit)
        void $ space
        inBag <- bag
        return (read num, inBag)

parseRules :: String -> [Rule]
parseRules s = fromRight [] $ parse ruleList "" s

allBags :: [Rule] -> [Bag]
allBags = nub . sort . concatMap bagsInRule
    where bagsInRule (outBag, inBags) = outBag : map snd inBags

bagMap :: [Rule] -> M.Map Bag Int
bagMap = M.fromList . (flip zip) [1..] . allBags 

containingBags :: Bag -> [Rule] -> [Bag]
containingBags b rules = map (nodeMap M.!) $ reachable (m M.! b) gr
    where
        m = bagMap rules
        nodes = map swap $ M.toList m
        ruleToEdges (outBag, inBags) =
            map (\(num,inBag) -> (m M.! inBag, m M.! outBag, num)) inBags
        edges = concatMap ruleToEdges rules
        gr = mkGraph nodes edges :: Gr Bag Int
        nodeMap = M.fromList nodes

--containedBags :: Bag -> [Rule] -> T.Tree (Int,Bag)
containedBags b rules = sumRec gr (m M.! b) - 1
    where
        m = bagMap rules
        nodes = map swap $ M.toList m
        ruleToEdges (outBag, inBags) =
            map (\(num,inBag) -> (m M.! outBag, m M.! inBag, num)) inBags
        edges = concatMap ruleToEdges rules
        gr = mkGraph nodes edges :: Gr Bag Int

sumRec :: Gr Bag Int -> Int -> Int
sumRec gr node = 1 + (sum $ map (\(x,w) -> w * (sumRec gr x)) $ lsuc gr node)


part1 :: String -> String
part1 = show . length . tail . containingBags "shiny-gold" . parseRules

part2 :: String -> String
part2 = show . containedBags "shiny-gold" . parseRules

main :: IO ()
main = interact $ part2

{-
 - vim: ts=4 expandtab
 -}
