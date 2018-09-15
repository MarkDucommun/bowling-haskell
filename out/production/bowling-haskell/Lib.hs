module Lib (
  score
  , constructGame
  , Frame(Strike, Spare, NormalFrame)
  , buildGame
  , lastFrame
  , lastFram
  , oneFrameGame
) where

score :: [Int] -> Maybe Int
score rolls =
  if any (\x -> x > 10 || x < 0) rolls
  then Nothing
  else scoreGame $ constructGame rolls 10

scoreGame :: Maybe Game -> Maybe Int
scoreGame Nothing = Nothing
scoreGame (Just game) = Just $ scoreGameInner game

scoreGameInner :: Game -> Int
scoreGameInner (Game Nothing last) = scoreLastFrame last
scoreGameInner (Game (Just frames) last) = scoreFramesInner frames last 0

scoreLastFrame :: LastFrame -> Int
scoreLastFrame (LastFrame x y Nothing) = x + y
scoreLastFrame (LastFrame x y (Just z)) = x + y + z

frameValue :: Frame -> Int
frameValue Strike = 10
frameValue (Spare _ _) = 10
frameValue (NormalFrame x y) = x + y

firstRollValue :: Frame -> Int
firstRollValue Strike = 10
firstRollValue (Spare x _) = x
firstRollValue (NormalFrame x _) = x

scoreFramesInner :: Frames -> LastFrame -> Int -> Int
scoreFramesInner [] last score = scoreLastFrame last + score
scoreFramesInner (Strike:n) last score = scoreStrike n last score
scoreFramesInner (Spare _ _:m) last score = scoreSpare m last score
scoreFramesInner (frame:o) last score = scoreFramesInner o last $ score + frameValue frame

scoreStrike :: Frames -> LastFrame -> Int -> Int
scoreStrike [] (LastFrame x y z) score = score + 10 + x + y + (scoreLastFrame $ LastFrame x y z)
scoreStrike (Strike:[]) (LastFrame x y z) score = scoreStrike [] (LastFrame x y z) $ score + 10 + 10 + x
scoreStrike (Strike:m:n) last score = scoreStrike ([m] ++ n) last $ score + 10 + 10 + firstRollValue m
scoreStrike (m:n) last score = scoreFramesInner ([m] ++ n) last $ score + 10 + frameValue m

scoreSpare :: Frames -> LastFrame -> Int -> Int
scoreSpare [] (LastFrame x y z) score = scoreFramesInner [] (LastFrame x y z) $ score + 10 + x
scoreSpare (m:n) last score = scoreFramesInner ([m] ++ n) last $ score + 10 + firstRollValue m

data Frame = Strike | Spare Int Int | NormalFrame Int Int deriving (Show, Eq)
data LastFrame = LastFrame Int Int (Maybe Int) deriving (Show, Eq)
type Frames = [Frame]
type GameLength = Int
data Game = Game (Maybe Frames) LastFrame deriving (Show, Eq)

buildGame :: Frames -> LastFrame -> Maybe Game
buildGame frames last = Just $ Game (Just frames) last

oneFrameGame :: LastFrame -> Maybe Game
oneFrameGame frame = Just $ Game Nothing frame

lastFrame :: Int -> Int -> Int -> LastFrame
lastFrame x y z = LastFrame x y $ Just z

lastFram :: Int -> Int -> LastFrame
lastFram x y = LastFrame x y Nothing

constructGame :: [Int] -> Int -> Maybe Game
constructGame [] n = Nothing
constructGame rolls 1 = lastFrameToGame $ constructLastFrame rolls
constructGame rolls len = constructNormalGame rolls len []

constructLastFrame :: [Int] -> Maybe LastFrame
constructLastFrame (10:y:z:[]) = Just $ LastFrame 10 y $ Just z
constructLastFrame (x:y:z:[]) =
  if x + y == 10
  then Just $ lastFrame x y z
  else Nothing
constructLastFrame (x:y:[]) =
  if x + y < 10
  then Just $ lastFram x y
  else Nothing

lastFrameToGame :: Maybe LastFrame -> Maybe Game
lastFrameToGame frame = aybe frame $ \last -> oneFrameGame last

framesAndMaybeLastFrameToGame :: Frames -> Maybe LastFrame -> Maybe Game
framesAndMaybeLastFrameToGame frames lastFrame = aybe lastFrame (\last -> buildGame frames last)

constructNormalGame :: [Int] -> GameLength -> [Frame] -> Maybe Game
constructNormalGame [] len frames = Nothing
constructNormalGame rolls len frames =
  if length frames == (len - 1)
  then framesAndMaybeLastFrameToGame frames $ constructLastFrame rolls
  else constructNonLastFrame rolls len frames

constructNonLastFrame :: [Int] -> GameLength -> [Frame] -> Maybe Game
constructNonLastFrame (_:[]) _ _ = Nothing
constructNonLastFrame (_:_:[]) _ _ = Nothing
constructNonLastFrame (10:rolls) len frames = constructNormalGame rolls len $ frames ++ [Strike]
constructNonLastFrame (x:y:rolls) len frames =
  if x + y == 10
  then constructNormalGame rolls len $ frames ++ [Spare x y]
  else constructNormalGame rolls len $ frames ++ [NormalFrame x y]

aybe :: Maybe a -> (a -> Maybe b) -> Maybe b
aybe Nothing _ = Nothing
aybe (Just a) fn = fn a
