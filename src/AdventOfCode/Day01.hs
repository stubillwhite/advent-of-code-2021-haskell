module AdventOfCode.Day01
  ( day01, solutionOne, solutionTwo
  ) where 

import Data.List ( find )
import Data.Maybe ( fromJust )
import Text.Parsec (count)

exampleInput :: String
exampleInput = unlines (map show [199, 200, 208, 210, 200, 207, 240, 269, 260, 263])

-- >>> slidingWindow 1 1 [1, 2, 3]
-- >>> slidingWindow 2 1 [1, 2, 3]
-- >>> slidingWindow 3 1 [1, 2, 3]
-- >>> slidingWindow 1 2 [1, 2, 3]
-- [[1],[2],[3]]
-- [[1,2],[2,3]]
-- [[1,2,3]]
-- [[1],[3]]
slidingWindow :: Int -> Int -> [a] -> [[a]]
slidingWindow size step [] = []
slidingWindow size step list = 
  if size < length list
    then take size list : slidingWindow size step (drop step list) 
    else [list]

-- >>> parseInput exampleInput
-- [199,200,208,210,200,207,240,269,260,263]
parseInput :: String -> [Int]
parseInput = map read . lines

-- >>> totalIncrements []
-- >>> totalIncrements [1, 2, 1]
-- >>> totalIncrements [1, 2, 2]
-- >>> totalIncrements [1, 2, 3]
-- 0
-- 1
-- 1
-- 2
totalIncrements :: [Int] -> Int 
totalIncrements (x:y:xs) = (if x < y then 1 else 0) + totalIncrements (y:xs)
totalIncrements _ = 0

-- >>> solutionOne exampleInput
-- 7
solutionOne :: String -> Int
solutionOne input = 
    totalIncrements parsedInput 
  where
    parsedInput = parseInput input
    
-- >>> solutionTwo exampleInput
-- 5
solutionTwo :: String -> Int
solutionTwo input = 
    totalIncrements (windowedSum parsedInput)
  where
    windowedSum = map sum . slidingWindow 3 1
    parsedInput = parseInput input
    
day01 :: IO ()
day01 = do  
    problemInput <- readFile "input/day-1-input.txt"
    putStrLn "Day 1"
    putStrLn $ "  Part one: " ++ show (solutionOne problemInput)
    putStrLn $ "  Part two: " ++ show (solutionTwo problemInput)
