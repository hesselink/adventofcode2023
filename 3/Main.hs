module Main where

import Data.Char (isDigit)
import Data.Map (Map)
import Data.List (unfoldr, nub)
import Data.Maybe (mapMaybe)

import qualified Data.Map as Map

import Lib.Point

main :: IO ()
main = do
  f <- readFile "input/3"
  let schematic = parse f
      numbers = extractNumbers f
      partNumbers = filter (isPartNumber schematic) numbers
      result = sum . map value $ partNumbers
  print result
  let gs = potentialGears schematic
      ns = numberMap numbers
      result2 = sum . mapMaybe (gearRatio ns) $ gs
  print result2

type Schematic = Map Point Char

parse :: String -> Schematic
parse = parseGrid id

data Number = Number
  { value :: Int
  , coordinates :: [Point]
  } deriving (Show, Eq)

extractNumbers :: String -> [Number]
extractNumbers = concatMap (uncurry extractNumbers1) . zip [0..] . lines

extractNumbers1 :: Int -> String -> [Number]
extractNumbers1 y line = unfoldr (uncurry (takeOneNumber y)) (0, line)

takeOneNumber :: Int -> Int -> String -> Maybe (Number, (Int, String))
takeOneNumber y x line =
  let (dropped, rest) = span (not . isDigit) line
      (ds, rest2) = span isDigit rest
      startX = x + length dropped
  in if null ds
     then Nothing
     else Just (Number (read ds) [ (x2, y) | x2 <- [startX .. startX + length ds - 1]], (startX + length ds, rest2))

isPartNumber :: Schematic -> Number -> Bool
isPartNumber sch n =
  let ns = concatMap neighbours (coordinates n)
  in any (isSymbol sch) ns

isSymbol :: Schematic -> Point -> Bool
isSymbol sch p = maybe False isSymbolChar (Map.lookup p sch)

isSymbolChar :: Char -> Bool
isSymbolChar c = not (isDigit c || c == '.')

potentialGears :: Schematic -> [Point]
potentialGears = map fst . filter ((== '*') . snd) . Map.toList

numberMap :: [Number] -> Map Point Number
numberMap = Map.fromList . concatMap (\n -> [(p, n) | p <- coordinates n])

gearRatio :: Map Point Number -> Point -> Maybe Int
gearRatio numbers p =
  let surroundingNumbers = nub $ mapMaybe (flip Map.lookup numbers) (neighbours p)
  in case surroundingNumbers of
    [n1, n2] -> Just $ value n1 * value n2
    _ -> Nothing
