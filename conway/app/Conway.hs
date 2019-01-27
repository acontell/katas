module Conway where

import System.Random (randomRIO)
import Control.Monad (replicateM)

type Size = Int
type Universe = [[Int]]

initialUniverse :: Size -> IO (Universe)
initialUniverse size = buildUniverse size size

buildUniverse :: Size -> Size -> IO (Universe)
buildUniverse _ 0 = return []
buildUniverse size x = do
                      r <- randomList size
                      rs <- buildUniverse size (x-1)
                      return (r:rs)

randomList :: Int -> IO ([Int])
randomList = ($ randomRIO (0,1::Int)) . replicateM

conway :: Size -> IO ([Universe])
conway size = do
                firstStage <- initialUniverse size
                return (iterate evolve firstStage)

evolve :: Universe -> Universe
evolve = id
