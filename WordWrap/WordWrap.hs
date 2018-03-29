module WordWrap where

import Prelude hiding (getLine)

-- General use functions
split :: Char -> [Char] -> [[Char]]
split _ [] = [[]]
split c (x:xs)
	| x == c = [[]] ++ rest
	| otherwise = word ++ tail rest
	where
		rest = split c xs
		word = [x:head rest]

-- str = glue (split str)
glue :: Char -> [[Char]] -> [Char]
glue _ [x] = x
glue c (x:xs) = x ++ c:glue c xs

-- Aliases
type Text = [Char]
type Word = [Char]
type Line = [Word]

-- Constants
lineSeparator :: Char
lineSeparator = '\n'

wordSeparator :: Char
wordSeparator = ' '

-- Functions
splitInWords :: Text -> [Word]
splitInWords = split wordSeparator

glueWords :: [Word] -> Text
glueWords = glue wordSeparator

splitInLines :: Int -> [Word] -> [Line]
splitInLines _ [] = []
splitInLines size ws = [line] ++ splitInLines size rest
	where
		line = getLine size ws
		rest = drop (length line) ws

getWordsAndSize :: [Word] -> [([Word], Int)]
getWordsAndSize = scanl getSizes ([], 0)
	where
		getSizes (ws, size) w = (ws ++ [w], size + length w)

getLine :: Int -> [Word] -> Line
getLine size ws = last getWordsThatFit
	where
		getWordsThatFit = [ws | (ws, _) <- takeWhile isInSize wordsAndSize]
		wordsAndSize = getWordsAndSize ws
		isInSize (_, s) = s <= size

glueLines :: [Line] -> Text
glueLines = glue lineSeparator . map glueWords

wrap :: Int -> Text -> Text
wrap size = glueLines . (splitInLines size) . splitInWords
