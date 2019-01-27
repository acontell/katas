module Conway where

import Control.Monad (replicateM)
import System.Random (randomRIO)

-- DATA STRUCTURES AND TYPES
data Cell = Dead | Alive deriving (Show, Eq)
type Size = Int
type Universe = [[Cell]]
type Position = (Int, Int)
type Neighbourhood = [Cell]
----------------------------

conway :: Size -> IO ([Universe])
conway size = do
                firstStage <- bigBang size
                return (iterate evolve firstStage)

-- BIG BANG!
bigBang :: Size -> IO (Universe)
bigBang size = create size size

create :: Size -> Int -> IO (Universe)
create _ 0 = return []
create size x = do
                  r <- listOfCells size
                  rs <- create size (x-1)
                  return (r:rs)

listOfCells :: Size -> IO ([Cell])
listOfCells = ($ randomCell) . replicateM

randomCell :: IO (Cell)
randomCell = fmap toCell (randomRIO (0,1::Int))
  where
    toCell 0 = Dead
    toCell 1 = Alive
--------------------

-- EVOLUTION CHAMBER
evolve :: Universe -> Universe
evolve universe = [[newState universe (column,row) | column <- [0..size]] | row <- [0..size]]
  where
    size = (length universe) - 1

newState :: Universe -> Position -> Cell
newState universe position
  | cell == Alive && (aliveNeighbours < 2 || aliveNeighbours > 3) = Dead
  | cell == Dead && aliveNeighbours == 3 = Alive
  | otherwise = cell
  where
    cell = find universe position
    aliveNeighbours = (length . filter (== Alive)) (neighbours universe position)

find :: Universe -> Position -> Cell
find universe (row, column)
  | betweenBounds row && betweenBounds column = (universe!!row)!!column
  | otherwise = Dead
  where
    betweenBounds x = x >= 0 && x < length universe

neighbours :: Universe -> Position -> Neighbourhood
neighbours universe (row, column) = map (find universe) [(row-1, column-1),(row-1, column),(row-1, column+1),(row, column-1),(row, column+1),(row+1, column-1),(row+1, column),(row+1, column+1)]
--------------------