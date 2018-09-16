module FrameLib (
  Frame(Strike, Spare, NormalFrame)
  , LastFrame(LastFrame)
  , Frames
  , Roll
  , frameValue
  , firstRollValue
  , constructLastFrame
) where

data Frame = Strike | Spare Int Int | NormalFrame Int Int deriving (Show, Eq)
data LastFrame = LastFrame Int Int (Maybe Int) deriving (Show, Eq)
type Frames = [Frame]
type Roll = Int

frameValue :: Frame -> Int
frameValue Strike = 10
frameValue (Spare _ _) = 10
frameValue (NormalFrame x y) = x + y

firstRollValue :: Frame -> Int
firstRollValue Strike = 10
firstRollValue (Spare x _) = x
firstRollValue (NormalFrame x _) = x

constructLastFrame :: [Roll] -> Maybe LastFrame
constructLastFrame (10:y:z:[]) = Just $ LastFrame 10 y $ Just z
constructLastFrame (x:y:z:[]) =
  if x + y == 10
  then Just $ LastFrame x y $ Just z
  else Nothing
constructLastFrame (x:y:[]) =
  if x + y < 10
  then Just $ LastFrame x y Nothing
  else Nothing