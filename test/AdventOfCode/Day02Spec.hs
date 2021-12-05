module AdventOfCode.Day02Spec (main, spec) where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )

import AdventOfCode.Day02 ( day02, solutionOne, solutionTwo )

main :: IO ()
main = hspec spec

exampleInput :: String
exampleInput =
  unlines
    [ "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    ]

spec :: Spec
spec = do
  describe "solutionOne" $ do
    it "should return expected result for example input" $ do
      solutionOne exampleInput `shouldBe` Right 150
  describe "solutionTwo" $ do
    it "should return expected result for example input" $ do
      solutionTwo exampleInput `shouldBe` Right 900
