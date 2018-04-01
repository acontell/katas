module Bowling where

import Data.Char

data Frame a = Strike | Spare a | Bonus a | Score (a, a)

type BowlingFrame = Frame Int

maxPoints :: Int
maxPoints = 10

toInt :: Char -> Int
toInt 'X' = maxPoints
toInt x = digitToInt x

-- Last word can transform into several frames.
wordToFrames :: String -> [BowlingFrame]
wordToFrames "X" = [Strike]
wordToFrames (a:_:[]) = [Spare (toInt a)]
wordToFrames (a:b:c:[])
	| b == '-' = [Score (x, z)]
	| b == '/' = [Spare x, Bonus z]
	| otherwise = [Strike, Bonus (toInt b), Bonus z]
	where
		x = toInt a
		z = toInt c

strToFrames :: String -> [BowlingFrame]
strToFrames = concat . map wordToFrames . words

getPointsOfOneBall :: BowlingFrame -> Int
getPointsOfOneBall Strike = maxPoints
getPointsOfOneBall (Spare x) = x
getPointsOfOneBall (Bonus x) = x
getPointsOfOneBall (Score (x, _)) = x

getPointsOfTwoBalls :: [BowlingFrame] -> Int
getPointsOfTwoBalls (Strike:x:_) = maxPoints + getPointsOfOneBall x
getPointsOfTwoBalls (Spare _:_) = maxPoints
getPointsOfTwoBalls (Bonus x:Bonus y:_) = x + y
getPointsOfTwoBalls (Score (x, y):_) = x + y

getPointsOfFrame :: BowlingFrame -> [BowlingFrame] -> Int
getPointsOfFrame (Bonus _) _ = 0
getPointsOfFrame (Score (x, y)) _ = x + y
getPointsOfFrame (Spare _) (x:_) = maxPoints + getPointsOfOneBall x
getPointsOfFrame Strike xs = maxPoints + getPointsOfTwoBalls xs

getPoints :: [BowlingFrame] -> Int
getPoints [] = 0
getPoints (x:xs) = getPointsOfFrame x xs + getPoints xs

bowling :: String -> Int
bowling = getPoints . strToFrames