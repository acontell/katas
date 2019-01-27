module Conway where

import System.Random (randomRIO)
import Control.Monad (replicateM)

data Cell = Dead | Alive
instance Show Cell where
    show Dead = show 0
    show Alive = show 1
instance Eq Cell where
    (==) Dead Dead = True
    (==) Alive Alive = True
    (==) _ _ = False

type Size = Int
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
