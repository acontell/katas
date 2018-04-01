module Bowling where

import Data.Char

data Frame a = Strike | Spare (a, a) | Score (a, a) | Bonus (a, a)

maxPoints :: Int
maxPoints = 10

toInt :: Char -> Int
toInt 'X' = maxPoints
toInt x = digitToInt x

strToFrames :: String -> [Frame Int]
strToFrames "X" = [Strike]
strToFrames (a:_:[]) = [Spare (x, maxPoints - x)]
	where
		x = toInt a
strToFrames (a:b:c:[])
	| b == '-' = [Score (x, z)]
	-- Bonuses are considered a new frame either of one ball (coming from a Spare) or of two balls (coming from a Strike). It's a new frame (Bonus) either way.
	| b == '/' = [Spare (x, maxPoints - x), Bonus (z, 0)]
	| otherwise = [Strike, Bonus (y, z)]
	where
		x = toInt a
		y = toInt b
		z = toInt c

toFrames :: String -> [Frame Int]
toFrames = concat . map strToFrames . words

getOneBall :: Frame Int -> Int
getOneBall Strike = maxPoints
getOneBall (Spare (x, _)) = x
getOneBall (Score (x, _)) = x
getOneBall (Bonus (x, _)) = x

getTwoBalls :: [Frame Int] -> Int
getTwoBalls (Strike:x:_) = maxPoints + getOneBall x
getTwoBalls ((Spare (x, y)):_) = maxPoints
getTwoBalls ((Score (x, y)):_) = x + y
getTwoBalls ((Bonus (x, y)):_) = x + y

getPoints :: Frame Int -> [Frame Int] -> Int
getPoints (Bonus (_, _)) _ = 0
getPoints (Score (x, y)) _ = x + y
getPoints (Spare (_, _)) (x:_) = maxPoints + getOneBall x
getPoints Strike xs = maxPoints + getTwoBalls xs

calculatePoints :: [Frame Int] -> Int
calculatePoints [] = 0
calculatePoints (x:xs) = getPoints x xs + calculatePoints xs

bowling :: String -> Int
bowling = calculatePoints . toFrames