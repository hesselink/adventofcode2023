module Lib.Parser where

import Control.Applicative
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

type Parser = P.Parsec Void String

runParser :: Parser a -> String -> a
runParser p = either (error . P.errorBundlePretty) id . P.runParser p "input"

parseNumber :: (Read a, Num a) => Parser a
parseNumber = read <$> many P.numberChar
