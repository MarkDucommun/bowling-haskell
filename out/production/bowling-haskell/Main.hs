module Main where

import Network
import Lib
import Control.Concurrent
import InputLib
import System.IO
import RequestParser
import ResponseCreator

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 8080
    loop sock

loop :: Socket -> IO()
loop sock = do
   (h,_,_) <- accept sock
   forkIO $ body h
   loop sock
  where
   body h = do
       msgLines <- readRequest h
       hPutStr h $ createResponse $ parseRequest msgLines
       hFlush h
       hClose h

readRequest :: Handle -> IO [String]
readRequest h = anotherReadRequest h "" [] Nothing

anotherReadRequest :: Handle -> String -> [String] -> Maybe Int -> IO [String]
anotherReadRequest h "\r" lines Nothing = return $ lines ++ ["\r"]
anotherReadRequest h "\r" lines (Just bodyCount) = do
  body <- getBody h bodyCount ""
  return $ lines ++ ["\r", body]
anotherReadRequest h lastLine lines Nothing = do
  nextLine <- hGetLine h
  anotherReadRequest h nextLine (lines ++ [lastLine]) $ getContentLength nextLine
anotherReadRequest h lastLine lines bodyCount = do
  nextLine <- hGetLine h
  anotherReadRequest h nextLine (lines ++ [lastLine]) bodyCount

getContentLength :: String -> Maybe Int
getContentLength line
  | headerType == "Content-Length" = parseString $ split line ':' !! 1
  | otherwise = Nothing
  where headerType = split line ':' !! 0

getBody :: Handle -> Int -> String -> IO String
getBody h count accumulator
  | count == 0 = return accumulator
  | otherwise = do
    next <- hGetChar h
    getBody h (count - 1) $ accumulator ++ [next]

--main :: IO ()
--main = do
--  putStrLn "HELLO, PLEASE GIVE ME BOWLING ROLLS"
--  rollsString <- getLine
--  calculateAndPrintScore $ parseInput rollsString

calculateAndPrintScore :: Maybe [Int] -> IO()
calculateAndPrintScore Nothing = putStrLn "SOMETHING YOU ENTERED WAS NOT A NUMBER BETWEEN 0 AND 10"
calculateAndPrintScore (Just rolls) = printScore $ score rolls

printScore :: Maybe Int -> IO()
printScore Nothing = putStrLn "SOMETHING ABOUT THE ROLLS YOU ENTERED WENT WRONG"
printScore (Just yourScore) = putStrLn ("YOU SCORED " ++ show yourScore)