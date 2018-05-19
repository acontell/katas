module GreetingTests where

import Greeting

import Test.Hspec

main :: IO ()
main = hspec spec

-- SPEC: https://github.com/testdouble/contributing-tests/wiki/Greeting-Kata
-- NOTICE that there's no requirement on what to do when there's more than one shouted name thus the example provided has only been considered.

spec :: Spec
spec = do
  describe "Greeting" $ do
    it "should interpolate name in a simple greeting." $ do
      greet "Bob" `shouldBe` "Hello, Bob."
    it "should handle nulls by introducing a stand-in (my friend)" $ do
      greet "" `shouldBe` "Hello, my friend."
    it "should handle shouting." $ do
      greet "JERRY" `shouldBe` "HELLO JERRY!"
    it "should handle two names of input." $ do
      greet ["Jill", "Jane"] `shouldBe` "Hello, Jill and Jane."
    it "should handle arbitrarily names of input." $ do
      greet ["Amy", "Brian", "Charlotte"] `shouldBe` "Hello, Amy, Brian, and Charlotte."
    it "should allow mixing of normal and shouted names by separating the response into two greetings." $ do
      greet ["Amy", "BRIAN", "Charlotte"] `shouldBe` "Hello, Amy and Charlotte. AND HELLO BRIAN!"
    it "if any entries in name are a string containing a comma, it should split it as its own input." $ do
      greet ["Bob", "Charlie, Dianne"] `shouldBe` "Hello, Bob, Charlie, and Dianne."
    it "should allow the input to escape intentional commas introduced by Requirement 7." $ do
      greet ["Bob", "'Charlie, Dianne'"] `shouldBe` "Hello, Bob and Charlie, Dianne."
    