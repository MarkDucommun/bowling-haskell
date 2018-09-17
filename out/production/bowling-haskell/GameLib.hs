module GameLib(
  Game(Game)
  , GameLength
  , Frame(Strike, Spare, NormalFrame)
  , LastFrame(LastFrame)
  , constructGame
) where

import FrameLib

type GameLength = Int
data Game = Game (Maybe Frames) LastFrame deriving (Show, Eq)

constructGame :: [Roll] -> GameLength -> Maybe Game
constructGame [] n = Nothing
constructGame rolls 1 = lastFrameToGame $ constructLastFrame rolls
constructGame rolls len = constructNormalGame rolls len []

constructNormalGame :: [Roll] -> GameLength -> [Frame] -> Maybe Game
constructNormalGame [] len frames = Nothing
constructNormalGame rolls len frames =
  if length frames == (len - 1)
  then framesAndMaybeLastFrameToGame frames $ constructLastFrame rolls
  else constructNonLastFrame rolls len frames

constructNonLastFrame :: [Roll] -> GameLength -> [Frame] -> Maybe Game
constructNonLastFrame (_:[]) _ _ = Nothing
constructNonLastFrame (_:_:[]) _ _ = Nothing
constructNonLastFrame (10:rolls) len frames = constructNormalGame rolls len $ frames ++ [Strike]
constructNonLastFrame (x:y:rolls) len frames =
  if x + y == 10
  then constructNormalGame rolls len $ frames ++ [Spare x y]
  else constructNormalGame rolls len $ frames ++ [NormalFrame x y]

lastFrameToGame :: Maybe LastFrame -> Maybe Game
lastFrameToGame frame = frame >>= \last -> Just $ Game Nothing last

framesAndMaybeLastFrameToGame :: Frames -> Maybe LastFrame -> Maybe Game
framesAndMaybeLastFrameToGame frames lastFrame = lastFrame >>= \last -> Just (Game (Just frames) last)
