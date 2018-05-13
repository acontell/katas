module DiamondTests where

import Diamond

import Test.Hspec

main :: IO ()
main = hspec spec

-- PRECONDITIONS: Only works with capital letters (ascii symbols [65...90]). Other symbols are not controlled.

-- SPEC:
--
-- Given a letter, print a diamond starting with ‘A’ with the supplied letter at the widest point.

-- For example: print-diamond ‘C’ prints

--   A
--  B B
-- C   C
--  B B
--   A

spec :: Spec
spec = do
  describe "Diamond" $ do
    it "returns diamond of initial letter" $ do
      diamond 'A' `shouldBe` "A"

    it "returns diamond of letter D" $ do
      diamond 'D' `shouldBe` "   A\n  B B\n C   C\nD     D\n C   C\n  B B\n   A"