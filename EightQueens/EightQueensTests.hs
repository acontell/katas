module EightQueensTests where

import EightQueens

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "EightQueens" $ do
    --oxxxxxxx
    --xxoxxxxx
    --xxxxoxxx
    --xxxxxx?x
    --xxxxxxxx
    --xxxxxxxx
    --xxxxxxxx
    --xxxxxxxx
    it "should check if a given position is not in check (left diagonal)" $ do
      isNotInCheckLeftDiagonal [0, 2, 4] 6 `shouldBe` True

    --oxxxxxxx
    --xxoxxxxx
    --xxxxoxxx
    --xxxxx?xx
    --xxxxxxxx
    --xxxxxxxx
    --xxxxxxxx
    --xxxxxxxx
    it "should check if a given position is not in check (left diagonal)" $ do
      isNotInCheckLeftDiagonal [0, 2, 4] 5 `shouldBe` False

    --oxxxxxxx
    --xxxoxxxx
    --xxxxxoxx
    --xxxx?xxx
    --xxxxxxxx
    --xxxxxxxx
    --xxxxxxxx
    --xxxxxxxx
    it "should check if a given position is not in check (left diagonal)" $ do
      isNotInCheckLeftDiagonal [0, 3, 5] 4 `shouldBe` True

    -- xxoxxxxx
    -- xxxxxoxx
    -- xxxxxxxo
    -- ?xxxxxxx
    -- xxxxxxxx
    -- xxxxxxxx
    -- xxxxxxxx
    -- xxxxxxxx
    it "should check if a given position is not in check (right diagonal)" $ do
      isNotInCheckRightDiagonal [2, 5, 7] 0 `shouldBe` True

    --xxxxxxox
    --xxxxoxxx
    --xxxxxxxo
    --xxxxxoxx
    --xxxx?xxx
    --xxxxxxxx
    --xxxxxxxx
    --xxxxxxxx
    it "should check if a given position is not in check (right diagonal)" $ do
      isNotInCheckRightDiagonal [6, 4, 7, 5] 4 `shouldBe` False

    -- xxxxxxox
    -- xxxxxxxo
    -- xxxxxoxx
    -- xxxoxxxx
    -- xxxxoxxx
    -- ?xxxxxxxx
    -- xxxxxxxx
    -- xxxxxxxx
    it "should check if a given position is not in check (right diagonal)" $ do
      isNotInCheckRightDiagonal [6, 7, 5, 3, 4] 0 `shouldBe` True

    it "should check if a given position is not in check (horizontal)" $ do
      -- Due to the algorithm this check doesn't make sense
      isNotInCheckHorizontal [6, 7, 5, 3, 4] 2 `shouldBe` True

    --xxxxxxox
    --xxxxxxxo
    --xxxxxoxx
    --xxxoxxxx
    --xxxxoxxx
    --xxxx?xxx
    --xxxxxxxx
    --xxxxxxxx
    it "should check if a given position is not in check (vertical)" $ do
      isNotInCheckVertical [6, 7, 8, 3, 4] 2 `shouldBe` True

    --xxxxxxox
    --xxxxxxxo
    --xxxxxoxx
    --xxxoxxxx
    --xxxxoxxx
    --xxxx?xxx
    --xxxxxxxx
    --xxxxxxxx
    it "should check if a given position is not in check (any)" $ do
      isNotInCheck [6, 7, 5, 3, 4] 5 `shouldBe` False

    --xxxoxxxx
    --xxxxxoxx
    --oxxxxxxx
    --xxxxoxxx
    --xoxxxxxx
    --xxxxxxxo
    --xxoxxxxx
    --xxxxxxox
    it "should check if a given position is not in check (any)" $ do
      isNotInCheck [3, 5, 0, 4, 1, 7, 2] 6 `shouldBe` True

    it "should check position is valid when previous is empty" $ do
      isNotInCheck [] 7 `shouldBe` True

    it "should return valid array with the problem solution" $ do
      (head . solveQueens) 8 `shouldBe` [0, 4, 7, 5, 2, 6, 1, 3]

    it "should return valid array with the problem solution (Bird)" $ do
      (head . queensB) 8 `shouldBe` [1, 5, 8, 6, 3, 7, 2, 4]
