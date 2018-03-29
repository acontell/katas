module WordWrapTests where

import WordWrap

import Test.Hspec

main :: IO ()
main = hspec spec

-- SPEC:
--
-- You write a class called Wrapper, that has a single static function named wrap that takes two arguments, a string, and a column number. 
-- The function returns the string, but with line breaks inserted at just the right places to make sure that no line is longer than the column number. 
-- You try to break lines at word boundaries.

-- Like a word processor, break the line by replacing the last space in a line with a newline.

-- PRECONDITIONS (to shorten the kata):
-- 1. Input text without tabs or extra blank spaces, just words separated by one blank space
-- 2. All words have length <= column
spec :: Spec
spec = do
  describe "WordWrap" $ do
    it "doesn't break when text size is less than column size" $ do
      wrap 7 "foo bar" `shouldBe` "foo bar"
    it "breaks when text length is greater than column size maximizing line length" $ do
      wrap 6 "foo bar foo" `shouldBe` "foo bar\nfoo"
    it "tries to break lines at word boundaries without exceeding column size" $ do
      wrap 5 "foo bar foo" `shouldBe` "foo\nbar\nfoo"
