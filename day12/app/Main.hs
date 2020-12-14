module Main where

import Data.List

data Dir  = East | North | West | South
    deriving (Show)
data Ship = Ship { posX :: Int, posY :: Int, dir :: Dir }
    deriving (Show)
data ShipW = ShipW { wPosX :: Int, wPosY :: Int, wayX :: Int, wayY :: Int }
    deriving (Show)

initShip :: Ship
initShip = Ship { posX = 0, posY = 0, dir = East }

initShipW :: ShipW
initShipW = ShipW { wPosX = 0, wPosY = 0, wayX = 10, wayY = 1 }

move :: Dir -> Int -> Ship -> Ship
move East  z s = s { posX = posX s + z }
move North z s = s { posY = posY s + z }
move West  z s = s { posX = posX s - z }
move South z s = s { posY = posY s - z }

moveW :: Dir -> Int -> ShipW -> ShipW
moveW East  z s = s { wayX = wayX s + z }
moveW North z s = s { wayY = wayY s + z }
moveW West  z s = s { wayX = wayX s - z }
moveW South z s = s { wayY = wayY s - z }

forward :: Int -> Ship -> Ship
forward z s = move (dir s) z s

forwardW :: Int -> ShipW -> ShipW
forwardW z s = s { wPosX = wPosX s + z*(wayX s), wPosY = wPosY s + z*(wayY s) }

plus :: Int -> Dir -> Dir
plus 0 d = d
plus 1 East = North
plus 1 North = West
plus 1 West = South
plus 1 South = East
plus 2 d = plus 1 $ plus 1 d
plus 3 d = plus 2 $ plus 1 d
plus n d = plus (n `mod` 4) d

turn :: Int -> Ship -> Ship
turn angle s = s { dir  = plus (angle `div` 90) (dir s) }

plusW :: Int -> (Int,Int) -> (Int,Int)
plusW 0 d     = d
plusW 1 (x,y) = (-y,x)
plusW 2 (x,y) = (-x,-y)
plusW 3 (x,y) = (y,-x)
plusW n d     = plusW (n `mod` 4) d

turnW :: Int -> ShipW -> ShipW
turnW angle s = s { wayX = wx, wayY = wy } where
    (wx, wy) = plusW (angle `div` 90) (wayX s, wayY s)

exec :: String -> Ship -> Ship
exec [] = id
exec (inst:arg) = let v = read arg in case inst of
    'N' -> move North v
    'S' -> move South v
    'E' -> move East  v
    'W' -> move West  v
    'F' -> forward v
    'L' -> turn v
    'R' -> turn (360-v)

execW :: String -> ShipW -> ShipW
execW [] = id
execW (inst:arg) = let v = read arg in case inst of
    'N' -> moveW North v
    'S' -> moveW South v
    'E' -> moveW East  v
    'W' -> moveW West  v
    'F' -> forwardW v
    'L' -> turnW v
    'R' -> turnW (360-v)

manhattan :: Ship -> Int
manhattan s = abs (posX s) + abs (posY s)

manhattanW :: ShipW -> Int
manhattanW s = abs (wPosX s) + abs (wPosY s)

part1 :: String -> String
part1 = show . manhattan . foldr ($!) initShip . map exec . reverse . lines

part2 :: String -> String
part2 = show . manhattanW . foldr ($!) initShipW . map execW . reverse . lines

main :: IO ()
main = interact $ part2
