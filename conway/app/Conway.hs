module Conway where

import System.Random (randomRIO)
import Control.Monad (replicateM)

data Cell = Dead | Alive deriving (Show, Eq)
type Size = Int
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
evolve = id

getNewState :: Cell -> Neighbourhood -> Cell
getNewState cell neighbours
  | cell == Alive && (alive < 2 || alive > 3) = Dead
  | cell == Dead && alive == 3 = Alive
  | otherwise = cell
  where
    alive = numberOfAliveCells neighbours

numberOfAliveCells :: Neighbourhood -> Int
numberOfAliveCells = length . filter (== Alive)