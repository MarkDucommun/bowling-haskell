module Lib (
  score
  , Frame(Strike, Spare, NormalFrame)
  , LastFrame(LastFrame)
  , Game(Game)
) where

import FrameLib
import GameLib

type Score = Int

score :: [Roll] -> Maybe Score
score rolls =
  if any (\roll -> roll > 10 || roll < 0) rolls
  then Nothing
  else scoreGame $ constructGame rolls 10

scoreGame :: Maybe Game -> Maybe Score
scoreGame Nothing = Nothing
scoreGame (Just game) = Just $ scoreGameInner game

scoreGameInner :: Game -> Score
scoreGameInner (Game Nothing last) = scoreLastFrame last
scoreGameInner (Game (Just frames) last) = scoreFramesInner frames last 0

scoreFramesInner :: Frames -> LastFrame -> Score -> Score
scoreFramesInner [] last score = scoreLastFrame last + score
scoreFramesInner (Strike:n) last score = scoreStrike n last score
scoreFramesInner (Spare _ _:m) last score = scoreSpare m last score
scoreFramesInner (frame:o) last score = scoreFramesInner o last $ score + frameValue frame

scoreStrike :: Frames -> LastFrame -> Score -> Score
scoreStrike [] (LastFrame x y z) score = score + 10 + x + y + (scoreLastFrame $ LastFrame x y z)
scoreStrike (Strike:[]) (LastFrame x y z) score = scoreStrike [] (LastFrame x y z) $ score + 10 + 10 + x
scoreStrike (Strike:m:n) last score = scoreStrike ([m] ++ n) last $ score + 10 + 10 + firstRollValue m
scoreStrike (m:n) last score = scoreFramesInner ([m] ++ n) last $ score + 10 + frameValue m

scoreSpare :: Frames -> LastFrame -> Score -> Score
scoreSpare [] (LastFrame x y z) score = scoreFramesInner [] (LastFrame x y z) $ score + 10 + x
scoreSpare (m:n) last score = scoreFramesInner ([m] ++ n) last $ score + 10 + firstRollValue m

scoreLastFrame :: LastFrame -> Score
scoreLastFrame (LastFrame x y Nothing) = x + y
scoreLastFrame (LastFrame x y (Just z)) = x + y + z
