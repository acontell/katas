{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Greeting where

import Data.Char
import Data.List
import Data.List.Split

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

isStrUpperCase :: String -> Bool
isStrUpperCase = all isUpper

instance Greet [String] where
	greet = sayHellos . separateUpperLower . trim . splitNames
		where
			splitNames = concat . map splitName
			trim = map (dropWhile isSpace)
			separateUpperLower names = (filter (not . isStrUpperCase) names, filter isStrUpperCase names)

splitName :: String -> [String]
splitName s
	| head s == '\'' && last s == '\'' = [(init . tail) s]
	| otherwise = splitOn "," s

sayHellos :: ([String], [String]) -> String
sayHellos (lowerNames, upperNames) = (sayHello . findOutNames) lowerNames ++ sayHelloToUpperName upperNames

sayHelloToUpperName :: [String] -> String
sayHelloToUpperName upperNames
	| length upperNames > 0 = " AND " ++ (greet . head) upperNames
	| otherwise = ""

findOutNames :: [String] -> String
findOutNames xs
	| length xs == 2 = head xs ++ " and " ++ last xs
	| otherwise = intercalate ", " (init xs ++ ["and " ++ last xs])
