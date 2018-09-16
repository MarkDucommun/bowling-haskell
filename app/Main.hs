module Main where

import Lib
import InputLib

main :: IO ()
main = do
  putStrLn "HELLO, PLEASE GIVE ME BOWLING ROLLS"
  rollsString <- getLine
  calculateAndPrintScore $ parseInput rollsString

calculateAndPrintScore :: Maybe [Int] -> IO()
calculateAndPrintScore Nothing = putStrLn "SOMETHING YOU ENTERED WAS NOT A NUMBER BETWEEN 0 AND 10"
calculateAndPrintScore (Just rolls) = printScore $ score rolls

printScore :: Maybe Int -> IO()
printScore Nothing = putStrLn "SOMETHING ABOUT THE ROLLS YOU ENTERED WENT WRONG"
printScore (Just score) = putStrLn ("YOU SCORED " ++ show score)