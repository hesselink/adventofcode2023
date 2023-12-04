module Main where

import Data.List (sort, group)

main :: IO ()
main = do
  f <- readFile "input/4"
  let cards = parse f
      result = sum . map (points . winningNumbers) $ cards
  print result

data Card = Card
  { winning :: [Int]
  , numbers :: [Int]
  } deriving (Show, Eq)

parse :: String -> [Card]
parse = map parse1 . lines

parse1 :: String -> Card
parse1 str =
  let (_, ':':' ':rest) = break (== ':') str
      (wins, '|':' ':nums) = break (== '|') rest
  in Card
    { winning = map read . words $ wins
    , numbers = map read . words $ nums
    }

winningNumbers :: Card -> [Int]
winningNumbers c = map head . filter ((== 2) . length) . group . sort $ winning c ++ numbers c

points :: [Int] -> Int
points [] = 0
points wins = 2 ^ (length wins - 1)
