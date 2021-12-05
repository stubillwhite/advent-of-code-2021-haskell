module AdventOfCode.Day01Spec (main, spec) where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )

import AdventOfCode.Day01 ( day01, solutionOne, solutionTwo )

main :: IO ()
main = hspec spec

exampleInput :: String
exampleInput = unlines (map show [199, 200, 208, 210, 200, 207, 240, 269, 260, 263])

spec :: Spec
spec = do
  describe "solutionOne" $ do
    it "should return expected result for example input" $ do
      solutionOne exampleInput `shouldBe` 7
  describe "solutionTwo" $ do
    it "should return correct result for example input" $ do
      solutionTwo exampleInput `shouldBe` 5      