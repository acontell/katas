{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Greeting where

import Data.Char
import Data.List
import Data.List.Split

splitIt :: String -> [String]
splitIt s
	| head s == '\'' && last s == '\'' = [(init . tail) s]
	| otherwise = splitOn "," s

isStrUpperCase :: String -> Bool
isStrUpperCase = all isUpper

class Greet a where
  greet :: a -> String

instance Greet String where
	greet = sayHello . findOutName

findOutName :: String -> String
findOutName str
	| str == "" = "my friend"
	| otherwise = str

sayHello :: String -> String
sayHello name
	| isStrUpperCase name = "HELLO " ++ name ++ "!"
	| otherwise = "Hello, " ++ name ++ "."

instance Greet [String] where
	greet = sayHellos . divide . prepare

prepare :: [String] -> [String]
prepare = clearSpaces . separate
	where
		separate = concat . map splitIt
		clearSpaces = map (dropWhile isSpace)

divide :: [String] -> ([String], [String])
divide names = (filter (not . isStrUpperCase) names, filter isStrUpperCase names)

sayHellos :: ([String], [String]) -> String
sayHellos (lowerNames, upperNames)
		| length upperNames > 0 = (sayHello . findOutNames) lowerNames ++ " AND " ++ (greet . head) upperNames
		| otherwise = (sayHello . findOutNames) lowerNames

findOutNames :: [String] -> String
findOutNames xs
	| length xs == 2 = head xs ++ " and " ++ lastName
	| otherwise = intercalate ", " (init xs ++ ["and " ++ lastName])
	where
		lastName = last xs
