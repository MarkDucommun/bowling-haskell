module InputLib(
  parseInput
  , splitOnSpaces
  , split
  , parseString
  , join
) where

parseInput :: String -> Maybe [Int]
parseInput string = parseInputInner (split string ',') []

parseInputInner :: [String] -> [Int] -> Maybe [Int]
parseInputInner [] ints = Just ints
parseInputInner (x:xs) ints = do
  int <- parseString x
  parseInputInner xs (ints ++ [int])

splitOnSpaces :: String -> [String]
splitOnSpaces string = split string ' '

join :: [String] -> String
join array = inner array ""
  where
    inner [] accum = accum
    inner (x:xs) accum = inner xs $ accum ++ x

split :: String -> Char -> [String]
split string char = splitInner char string [] ""

splitInner :: Char -> String -> [String] -> String -> [String]
splitInner char (x:xs) array accumulator =
  if x == char
  then splitInner char xs (array ++ [accumulator]) ""
  else splitInner char xs array $ accumulator ++ [x]
splitInner _ [] array accumulator = array ++ [accumulator]

reverse' :: [a] -> [a]
reverse' list = inner list []
  where
    inner [] result = result
    inner (x:xs) result = inner xs ([x] ++ result)

parseString :: String -> Maybe Int
parseString string = parseStringInner (reverse' string) 1 0

parseStringInner :: String -> Int -> Int -> Maybe Int
parseStringInner [] _ result = Just result
parseStringInner (' ':xs) multiplier result = parseStringInner xs multiplier result
parseStringInner ('\r':xs) multiplier result = parseStringInner xs multiplier result
parseStringInner (char:remaining) multiplier result = do
  value <- parseChar char
  parseStringInner remaining (10 * multiplier) (result + (value * multiplier))

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