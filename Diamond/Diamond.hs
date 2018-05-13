module Diamond where

import Data.Char
import Data.List

-- Constant
asciiA :: Int
asciiA = 65

initialLetter :: String
initialLetter = "A"

-- Utility methods
repeatBlank :: Int -> String
repeatBlank n = take n (repeat ' ')

printDiamond :: Char -> IO ()
printDiamond = putStrLn . diamond

-- DSL
diamond :: Char -> String
diamond = toLines . buildDiamond
	where
		toLines = intercalate "\n"

buildDiamond :: Char -> [String]
buildDiamond = appendLowerTriangle . buildUpperTriangle

buildUpperTriangle :: Char -> [String]
buildUpperTriangle c = [blanksOf level ++ lettersOf level | level <- [0..height]]
	where
		height = (subtract asciiA . ord) c
		blanksOf level = repeatBlank (height - level)

lettersOf :: Int -> String
lettersOf 0 = initialLetter
lettersOf level = letter:blankSpaces ++ [letter]
	where
		letter = (chr . (+ asciiA)) level
		blankSpaces = repeatBlank (level * 2 - 1)

appendLowerTriangle :: [String] -> [String]
appendLowerTriangle l = l ++ tail (reverse l)
