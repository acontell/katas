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
