module ScoreBowlingGame
  ( scoreBowlingGame
  ) where

import           BowlingLib
import           InputLib
import           Server

scoreBowlingGame :: RequestHandler
scoreBowlingGame = GET "/bowling" $ Pure calculateScore
  where
    calculateScore Nothing = BAD_REQUEST $ Text "<h1 style='color: red'>No Frames Present</h1>"
    calculateScore (Just params) =
      case getParam params "rolls" of
        Nothing -> BAD_REQUEST $ Text "<h1 style='color: red'>No Rolls Present</h1>"
        (Just rollString) -> convertRollStringToScore rollString

convertRollStringToScore :: String -> Response
convertRollStringToScore rolls =
  case parseInput rolls of
    Nothing -> BAD_REQUEST $ Text "<h1 style='color: red'>Incorrect Roll Format</h1>"
    (Just rollList) -> scoreRolls rollList

scoreRolls :: [Int] -> Response
scoreRolls rolls =
  case score rolls of
    Nothing -> BAD_REQUEST $ Text "<h1 style='color: red'>Incorrect Roll Format</h1>"
    (Just theScore) -> OK $ Text $ "<h1 style='color: green'>Score: " ++ show theScore ++ "</h1>"
