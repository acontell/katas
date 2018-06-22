module EightQueens where

import Data.List

type Position = (Int, Int)

isNotInCheck :: [Int] -> Int -> Bool
isNotInCheck rows column =
	isNotInCheckHorizontal rows column
	&& isNotInCheckLeftDiagonal rows column
	&& isNotInCheckRightDiagonal rows column
	&& isNotInCheckVertical rows column

isNotInCheckHorizontal :: [Int] -> Int -> Bool
-- IMPOSSIBLE NOT TO BE NotInCheck: we're adding to an array which each position represents a row, no two ints can go in the same cell!
isNotInCheckHorizontal _ _ = True

isNotInCheckLeftDiagonal :: [Int] -> Int -> Bool
isNotInCheckLeftDiagonal = isNotInCheckDiagonal (-)

isNotInCheckRightDiagonal :: [Int] -> Int -> Bool
isNotInCheckRightDiagonal = isNotInCheckDiagonal (+)

isNotInCheckDiagonal :: (Int -> Int -> Int) -> [Int] -> Int -> Bool
isNotInCheckDiagonal op rows column = and [isPositionNotTakenByQueen pos | pos <- [1..row], row - pos >= 0]
	where
		row = length rows
		isPositionNotTakenByQueen pos = (op column) pos /= rows!!(row - pos)

isNotInCheckVertical :: [Int] -> Int -> Bool
isNotInCheckVertical queens column = not (column `elem` queens)

queens :: [Int] -> Int -> [[Int]]
queens prev 0 = [prev]
queens prev x = concat [queens (prev ++ [column]) (x - 1) | column <- [0..7], isNotInCheck prev column]

solveQueens :: Int -> [[Int]]
solveQueens = queens []
