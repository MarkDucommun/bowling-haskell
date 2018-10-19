module BowlingLib (
  score
  , Frame(Strike, Spare, NormalFrame)
  , LastFrame(LastFrame)
  , Game(Game)
) where

import           FrameLib
import           GameLib

type Score = Int

score :: [Roll] -> Maybe Score
score rolls =
  if any (\roll -> roll > 10 || roll < 0) rolls
  then Nothing
  else scoreGame $ constructGame rolls 10

scoreGame :: Maybe Game -> Maybe Score
scoreGame Nothing     = Nothing
scoreGame (Just game) = Just $ scoreGameInner game

scoreGameInner :: Game -> Score
scoreGameInner (Game Nothing lastFrame) = scoreLastFrame lastFrame
scoreGameInner (Game (Just frames) lastFrame) = scoreFramesInner frames lastFrame 0

scoreFramesInner :: Frames -> LastFrame -> Score -> Score
scoreFramesInner [] lastFrame scoreValue = scoreLastFrame lastFrame + scoreValue
scoreFramesInner (Strike:n) lastFrame scoreValue = scoreStrike n lastFrame scoreValue
scoreFramesInner (Spare _ _:m) lastFrame scoreValue = scoreSpare m lastFrame scoreValue
scoreFramesInner (frame:o) lastFrame scoreValue = scoreFramesInner o lastFrame $ scoreValue + frameValue frame

scoreStrike :: Frames -> LastFrame -> Score -> Score
scoreStrike [] (LastFrame x y z) scoreValue = scoreValue + 10 + x + y + (scoreLastFrame $ LastFrame x y z)
scoreStrike (Strike:[]) (LastFrame x y z) scoreValue = scoreStrike [] (LastFrame x y z) $ scoreValue + 10 + 10 + x
scoreStrike (Strike:m:n) lastFrame scoreValue = scoreStrike ([m] ++ n) lastFrame $ scoreValue + 10 + 10 + firstRollValue m
scoreStrike (m:n) lastFrame scoreValue = scoreFramesInner ([m] ++ n) lastFrame $ scoreValue + 10 + frameValue m

scoreSpare :: Frames -> LastFrame -> Score -> Score
scoreSpare [] (LastFrame x y z) scoreValue = scoreFramesInner [] (LastFrame x y z) $ scoreValue + 10 + x
scoreSpare (m:n) lastFrame scoreValue = scoreFramesInner ([m] ++ n) lastFrame $ scoreValue + 10 + firstRollValue m

scoreLastFrame :: LastFrame -> Score
scoreLastFrame (LastFrame x y Nothing)  = x + y
scoreLastFrame (LastFrame x y (Just z)) = x + y + z
