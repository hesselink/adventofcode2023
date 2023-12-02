{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Lib.Parser

main :: IO ()
main = do
  f <- readFile "input/2"
  let records = parse f
      possibleGames = filter isPossible records
      result = sum . map id_ $ possibleGames
  print result
  let minCubeSets = map minCubeSet records
      result2 = sum . map power $ minCubeSets
  print result2

data Record = Record
  { id_ :: Int
  , revealedSets :: [CubeSet]
  } deriving (Show, Eq)

data CubeSet = CubeSet
  { red :: Int
  , green :: Int
  , blue :: Int
  } deriving (Show, Eq)

instance Semigroup CubeSet where
  cs1 <> cs2 = CubeSet
    { red = red cs1 + red cs2
    , green = green cs1 + green cs2
    , blue = blue cs1 + blue cs2
    }

instance Monoid CubeSet where
  mempty = CubeSet 0 0 0

newtype MaxCubeSet = MaxCubeSet { unMaxCubeSet :: CubeSet }

instance Semigroup MaxCubeSet where
  MaxCubeSet cs1 <> MaxCubeSet cs2 = MaxCubeSet $ CubeSet
    { red = red cs1 `max` red cs2
    , green = green cs1 `max` green cs2
    , blue = blue cs1 `max` blue cs2
    }

instance Monoid MaxCubeSet where
  mempty = MaxCubeSet $ CubeSet 0 0 0

isPossible :: Record -> Bool
isPossible = all isPossible1 . revealedSets

isPossible1 :: CubeSet -> Bool
isPossible1 cs = red cs < 13 && green cs < 14 && blue cs < 15

minCubeSet :: Record -> CubeSet
minCubeSet = unMaxCubeSet . mconcat . map MaxCubeSet . revealedSets

power :: CubeSet -> Int
power cs = red cs * green cs * blue cs

parse :: String -> [Record]
parse = runParser pRecords

pRecords :: Parser [Record]
pRecords = many (pRecord <* P.newline)

pRecord :: Parser Record
pRecord = Record <$ P.string "Game " <*> parseNumber <* P.string ": " <*> pCubeSets

pCubeSets :: Parser [CubeSet]
pCubeSets = pCubeSet `P.sepBy` P.string "; "

pCubeSet :: Parser CubeSet
pCubeSet = mconcat <$> pDraw `P.sepBy` P.string ", "

pDraw :: Parser CubeSet
pDraw = mkCubeSet <$> parseNumber <* P.char ' ' <*> pColor
  where
    mkCubeSet n = \case
      "red" -> CubeSet n 0 0
      "green" -> CubeSet 0 n 0
      "blue" -> CubeSet 0 0 n
      c -> error $ "Unsupported color: " ++ c

pColor :: Parser String
pColor = P.string "red" <|> P.string "green" <|> P.string "blue"
