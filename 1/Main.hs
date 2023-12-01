{-# LANGUAGE ViewPatterns #-}
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, unfoldr)

main :: IO ()
main = do
  f <- readFile "input/1"
  let vs = parse f
      result = sum vs
  print result
  let vs2 = parse2 f
      result2 = sum vs2
  print result2

parse :: String -> [Int]
parse = map (parseCalibrationValue mkDigits) . lines
  where
    mkDigits = map digitToInt . filter isDigit

parseCalibrationValue :: (String -> [Int]) -> String -> Int
parseCalibrationValue mkDigits str =
  let digits = mkDigits str
      firstDigit = head digits
      lastDigit = last digits
  in 10 * firstDigit + lastDigit

parse2 :: String -> [Int]
parse2 = map (parseCalibrationValue toDigits) . lines

toDigits :: String -> [Int]
toDigits = unfoldr takeDigit

takeDigit :: String -> Maybe (Int, String)
takeDigit str@(c:rest)
  | isDigit c = Just (digitToInt c, rest)
  | "one" `isPrefixOf` str = Just (1, rest)
  | "two" `isPrefixOf` str = Just (2, rest)
  | "three" `isPrefixOf` str = Just (3, rest)
  | "four" `isPrefixOf` str = Just (4, rest)
  | "five" `isPrefixOf` str = Just (5, rest)
  | "six" `isPrefixOf` str = Just (6, rest)
  | "seven" `isPrefixOf` str = Just (7, rest)
  | "eight" `isPrefixOf` str = Just (8, rest)
  | "nine" `isPrefixOf` str = Just (9, rest)
  | otherwise = takeDigit rest
takeDigit [] = Nothing
