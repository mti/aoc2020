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

data Expr = Val Integer | Add Expr Expr | Mul Expr Expr | Paren Expr
    deriving Show

exprList :: Parser [Expr]
exprList = expr `sepEndBy` newline

blanks :: Parser ()
blanks = skipMany (char ' ')

expr :: Parser Expr
expr = (try add <|> try mul <|> paren <|> val) <* blanks

paren :: Parser Expr
paren = do
    void $ char '(' <* blanks
    e <- expr
    void $ char ')' <* blanks
    return (Paren e)

add :: Parser Expr
add = do
    v <- paren <|> val
    void $ char '+' <* blanks
    e <- expr
    return (Add v e)

mul :: Parser Expr
mul = do
    v <- paren <|> val
    void $ char '*' <* blanks
    e <- expr
    return (Mul v e)

val :: Parser Expr
val = do
    n <- many1 digit
    void $ blanks
    return (Val $ read n)

exprAdvList :: Parser [Expr]
exprAdvList = exprAdv `sepEndBy` newline

exprAdv :: Parser Expr
exprAdv = try mulAdv <|> exprAdvNoMul

exprAdvNoMul :: Parser Expr
exprAdvNoMul = try addAdv <|> parenAdv <|> val

parenAdv :: Parser Expr
parenAdv = do
    void $ char '(' <* blanks
    e <- exprAdv
    void $ char ')' <* blanks
    return (Paren e)

mulAdv :: Parser Expr
mulAdv = do
    e1 <- exprAdvNoMul
    void $ char '*' <* blanks
    e2 <- exprAdv
    void $ blanks
    return (Mul e1 e2)

addAdv :: Parser Expr
addAdv = do
    e1 <- parenAdv <|> val
    void $ char '+' <* blanks
    e2 <- exprAdvNoMul
    return (Add e1 e2)

eval :: Expr -> Integer
eval (Val n) = n
eval (Paren e) = eval e
eval (Add e1 (Add e2 e3)) = eval (Add (Val ((eval e1) + eval e2)) e3)
eval (Add e1 (Mul e2 e3)) = eval (Mul (Val ((eval e1) + eval e2)) e3)
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 (Add e2 e3)) = eval (Add (Val ((eval e1) * eval e2)) e3)
eval (Mul e1 (Mul e2 e3)) = eval (Mul (Val ((eval e1) * eval e2)) e3)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalAdv :: Expr -> Integer
evalAdv (Val n) = n
evalAdv (Paren e) = evalAdv e
evalAdv (Mul e1 e2) = (evalAdv e1) * (evalAdv e2)
evalAdv (Add e1 e2) = (evalAdv e1) + (evalAdv e2)

parseExprList :: String -> [Expr]
parseExprList = fromRight [] . parse exprList ""

parseExprAdvList :: String -> [Expr]
parseExprAdvList = fromRight [] . parse exprAdvList ""

part1 :: String -> String
part1 = show . sum . map (eval) . parseExprList

part2 :: String -> String
part2 = show . sum . map (evalAdv) . parseExprAdvList

main :: IO ()
main = interact $ part2

{-
 - vim: ts=4 expandtab
 -}
