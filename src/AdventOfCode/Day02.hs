module AdventOfCode.Day02
  ( day02,
    solutionOne, 
    solutionTwo
  )
where

import Data.List (find)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)

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

data Command
  = Forward Int
  | Up Int
  | Down Int
  deriving (Show)

-- >>> parse command "Failed to parse" "forward 23\n"
-- >>> parse command "Failed to parse" "up 23\n"
-- >>> parse command "Failed to parse" "down 23\n"
-- Right (Forward 23)
-- Right (Up 23)
-- Right (Down 23)
command :: Parser Command
command = do
  direction <- string "forward" <|> string "up" <|> string "down"
  space
  distance <- number
  endOfLine
  case direction of
    "forward" -> return $ Forward distance
    "up" -> return $ Up distance
    "down" -> return $ Down distance
    _ -> fail $ "Failed to parse" ++ direction
  where
    number = read <$> many1 digit

commands :: Parser [Command]
commands = many command

data Location = Location
  { position :: Int,
    depth :: Int,
    aim :: Int
  }
  deriving (Show)

updateLocation :: Location -> Command -> Location
updateLocation location@Location {position} (Forward n) = location {position = position + n}
updateLocation location@Location {depth} (Up n) = location {depth = depth - n}
updateLocation location@Location {depth} (Down n) = location {depth = depth + n}

productOfFinalPosition :: Location -> Int
productOfFinalPosition Location {position, depth} = position * depth

-- >>> solutionOne exampleInput
-- Right 150
solutionOne :: String -> Either ParseError Int
solutionOne input =
  fmap (productOfFinalPosition . foldl updateLocation (Location 0 0 0)) cmds
  where
    cmds = parse commands "Failed to parse" input

updateLocationAndAim :: Location -> Command -> Location
updateLocationAndAim location@Location {position, depth, aim} (Forward n) = location {position = position + n, depth = depth + aim * n}
updateLocationAndAim location@Location {aim} (Up n) = location {aim = aim - n}
updateLocationAndAim location@Location {aim} (Down n) = location {aim = aim + n}

-- >>> solutionTwo exampleInput
-- Right 900
solutionTwo :: String -> Either ParseError Int
solutionTwo input =
  fmap (productOfFinalPosition . foldl updateLocationAndAim (Location 0 0 0)) cmds
  where
    cmds = parse commands "Failed to parse" input

day02 :: IO ()
day02 = do
  problemInput <- readFile "input/day-2-input.txt"
  putStrLn "Day 2"
  putStrLn $ "  Part one: " ++ show (solutionOne problemInput)
  putStrLn $ "  Part two: " ++ show (solutionTwo problemInput)
