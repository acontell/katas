{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Greeting where

import Data.Char
import Data.List
import Data.List.Split

class Greet a where
  greet :: a -> String

isNameUpperCase :: String -> Bool
isNameUpperCase = all isUpper

greetName :: String -> String
greetName name
	| isNameUpperCase name = "HELLO " ++ name ++ "!"
	| otherwise = "Hello, " ++ name ++ "."

chooseName :: String -> String
chooseName "" = "my friend"
chooseName name = name

chooseNames :: [String] -> String
chooseNames xs
	| length xs == 2 = head xs ++ " " ++ lastName
	| otherwise = intercalate ", " (init xs ++ [lastName])
	where
		lastName = "and " ++ last xs

instance Greet String where
	greet = greetName . chooseName

instance Greet [String] where
	greet names
		| hasUpperNames = greetLowerCase ++ " AND " ++ greetUpperCase
		| otherwise = greetLowerCase
		where
			name = map (dropWhile isSpace) (concat (map splitIt names))
			splitIt s
				| head s == '\'' && last s == '\'' = [(init . tail) s]
				| otherwise = splitOn "," s
			hasUpperNames = (length . filter isNameUpperCase) name > 0
			greetLowerCase = (greet . chooseNames . (filter (not . isNameUpperCase))) name
			greetUpperCase = (greet . chooseName . head . (filter isNameUpperCase)) name