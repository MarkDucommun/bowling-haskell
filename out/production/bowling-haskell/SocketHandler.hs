module SocketHandler (
  readRequest
) where

import           InputLib
import           System.IO

readRequest :: Handle -> IO [String]
readRequest h = anotherReadRequest h "" [] Nothing

anotherReadRequest :: Handle -> String -> [String] -> Maybe Int -> IO [String]

anotherReadRequest _ "\r" requestLines Nothing = return $ requestLines ++ ["\r"]

anotherReadRequest h "\r" requestLines (Just bodyCount) = do
  body <- getBody h bodyCount ""
  return $ requestLines ++ ["\r", body]

anotherReadRequest h lastLine requestLines Nothing = do
  nextLine <- hGetLine h
  anotherReadRequest h nextLine (requestLines ++ [lastLine]) $ getContentLength nextLine

anotherReadRequest h lastLine requestLines bodyCount = do
  nextLine <- hGetLine h
  anotherReadRequest h nextLine (requestLines ++ [lastLine]) bodyCount

getContentLength :: String -> Maybe Int
getContentLength line
  | headerType == "Content-Length" = parseString $ split line ':' !! 1
  | otherwise = Nothing
  where
    headerType = split line ':' !! 0

getBody :: Handle -> Int -> String -> IO String
getBody h count accumulator
  | count == 0 = return accumulator
  | otherwise = do
    next <- hGetChar h
    getBody h (count - 1) $ accumulator ++ [next]
