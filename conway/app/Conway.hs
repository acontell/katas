module Conway where

import System.Random (randomRIO)
import Control.Monad (replicateM)

data Cell = Dead | Alive deriving (Show, Eq)
type Size = Int
type CellPosition = (Int, Int)
type Neighbourhood = [Cell]
type Universe = [[Cell]]

initialUniverse :: Size -> IO (Universe)
initialUniverse size = buildUniverse size size

buildUniverse :: Size -> Size -> IO (Universe)
buildUniverse _ 0 = return []
buildUniverse size x = do
                      r <- randomCellList size
                      rs <- buildUniverse size (x-1)
                      return (r:rs)

randomCellList :: Int -> IO ([Cell])
randomCellList = ($ getRandomCell) . replicateM

getRandomCell :: IO (Cell)
getRandomCell = fmap toCell (randomRIO (0,1::Int))

toCell :: Int -> Cell
toCell 0 = Dead
toCell 1 = Alive

conway :: Size -> IO ([Universe])
conway size = do
                firstStage <- initialUniverse size
                return (iterate evolve firstStage)

evolve :: Universe -> Universe
evolve universe = [[getNewState universe (column,row) | column <- [0..size]] | row <- [0..size]]
  where
    size = (length universe) - 1

getNewState :: Universe -> CellPosition -> Cell
getNewState universe cellPosition
  | cell == Alive && (aliveNeighbours < 2 || aliveNeighbours > 3) = Dead
  | cell == Dead && aliveNeighbours == 3 = Alive
  | otherwise = cell
  where
    cell = getCell universe cellPosition
    aliveNeighbours = (length . filter (== Alive)) (getNeighbours universe cellPosition)

getCell :: Universe -> CellPosition -> Cell
getCell universe (row, column)
  | betweenBounds row && betweenBounds column = (universe!!row)!!column
  | otherwise = Dead
  where
    betweenBounds x = x >= 0 && x < length universe

getNeighbours :: Universe -> CellPosition -> Neighbourhood
getNeighbours universe (row, column) = map (getCell universe) [(row-1, column-1),(row-1, column),(row-1, column+1),(row, column-1),(row, column+1),(row+1, column-1),(row+1, column),(row+1, column+1)]
