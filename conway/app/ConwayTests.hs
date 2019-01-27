module ConwayTests where

import Conway

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Conway" $ do
    it "returns initial universe with numbers of rows of predefined size" $ do
        firstUniverse <- initialUniverse 50
        (length firstUniverse) `shouldBe` 50
    it "returns initial universe with numbers of columns of predefined size" $ do
        firstUniverse <- initialUniverse 50
        (all (== 50) (map length firstUniverse)) `shouldBe` True
    it "returns different initial state everytime it's called" $ do
        firstUniverse <- initialUniverse 50
        secondUniverse <- initialUniverse 50
        firstUniverse `shouldNotBe` secondUniverse
--      1. Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
--      2. Any live cell with more than three live neighbours dies, as if by overcrowding.
--      3. Any live cell with two or three live neighbours lives on to the next generation.
--      4. Any dead cell with exactly three live neighbours becomes a live cell.
    it "returns dead when alive and less than two alive neighbours" $ do
        getNewState Alive [Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead] `shouldBe` Dead
    it "returns dead when alive and more than three alive neighbours" $ do
        getNewState Alive [Alive,Dead,Alive,Dead,Dead,Alive,Dead,Alive] `shouldBe` Dead
    it "returns alive when alive and exactly two alive neighbours" $ do
        getNewState Alive [Dead,Dead,Alive,Dead,Dead,Alive,Dead,Dead] `shouldBe` Alive
    it "returns alive when alive and exactly three alive neighbours" $ do
        getNewState Alive [Dead,Alive,Dead,Alive,Dead,Alive,Dead,Dead] `shouldBe` Alive
    it "returns alive when dead and exactly three alive neighbours" $ do
        getNewState Dead [Dead,Dead,Alive,Dead,Dead,Alive,Dead,Alive] `shouldBe` Alive
