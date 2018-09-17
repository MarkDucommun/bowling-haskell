module InputLib(
  parseInput
  , splitOnSpaces
  , parseString
) where

import Maybe

parseInput :: String -> Maybe [Int]
parseInput string = parseInputInner (splitOnSpaces string) []

parseInputInner :: [String] -> [Int] -> Maybe [Int]
parseInputInner [] ints = Just ints
parseInputInner (x:xs) ints = parseString x >>= \int -> parseInputInner xs (ints ++ [int])

splitOnSpaces :: String -> [String]
splitOnSpaces string = split string ' '

split :: String -> Char -> [String]
split string char = splitInner char string [] ""

splitInner :: Char -> String -> [String] -> String -> [String]
splitInner char (x:xs) array accumulator =
  if x == char
  then splitInner char xs (array ++ [accumulator]) ""
  else splitInner char xs array $ accumulator ++ [x]
splitInner _ [] array accumulator = array ++ [accumulator]

parseString :: String -> Maybe Int
parseString (x:[]) = parseChar x
parseString (x:y:[]) = parseChar x >>= \firstChar -> parseChar y >>= \secondChar -> Just $ firstChar * 10 + secondChar
parseString _ = Nothing

parseChar :: Char -> Maybe Int
parseChar '0' = Just 0
parseChar '1' = Just 1
parseChar '2' = Just 2
parseChar '3' = Just 3
parseChar '4' = Just 4
parseChar '5' = Just 5
parseChar '6' = Just 6
parseChar '7' = Just 7
parseChar '8' = Just 8
parseChar '9' = Just 9
parseChar _ = Nothing