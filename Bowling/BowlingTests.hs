module BowlingTests where

import Bowling

import Test.Hspec

main :: IO ()
main = hspec spec

-- SPEC:
--
-- Create a program, which, given a valid sequence of rolls for one line of American Ten-Pin Bowling, produces the total score for the game.
-- Here are some things that the program will not do:
-- 1. We will not check for valid rolls.
-- 2. We will not check for correct number of rolls and frames.
-- 3. We will not provide scores for intermediate frames.

spec :: Spec
spec = do
  describe "Bowling" $ do
    it "returns sum of both partials (no strike/spare)" $ do
      bowling "0-4 0-3 0-0 9-0 0-2 9-0 9-0 3-6 9-0 1-8" `shouldBe` 63
    
    it "adds the spare bonus correctly" $ do
      bowling "0-0 0-0 0-0 5/ 3/ 0-0 0-0 0-0 0-0 0-0" `shouldBe` 23

    it "adds the spare bonus of the last frame correctly" $ do
      bowling "0-0 0-0 0-0 0-0 0-0 0-0 0-0 0-0 0-0 5/5" `shouldBe` 15

    it "concatenates spare bonus correctly" $ do
      bowling "5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5" `shouldBe` 150

    it "adds the strike bonus correctly" $ do
      bowling "0-0 0-0 0-0 X 3/ 0-0 0-0 0-0 0-0 0-0" `shouldBe` 30
    
    it "adds the strike bonus of the last frame correctly" $ do
      bowling "0-0 0-0 0-0 0-0 0-0 0-0 0-0 0-0 0-0 XX5" `shouldBe` 25

    it "concatenates strikes correctly" $ do
      bowling "X X X X X X X X X XXX" `shouldBe` 300

    it "works well with mixed samples (1)" $ do
      bowling "X 3/ 6-1 X X X 2/ 9-0 7/ XXX" `shouldBe` 193

    it "works well with mixed samples (2)" $ do
      bowling "9-0 3-5 6-1 3-6 8-1 5-3 2-5 8-0 7-1 8-1" `shouldBe` 82

    it "works well with mixed samples (3)" $ do
      bowling "9-0 3/ 6-1 3/ 8-1 5/ 0/ 8-0 7/ 8/8" `shouldBe` 131
