module RequestParser (
  Request (GET, POST, PUT, DELETE)
  , parseRequest
) where

import InputLib

data Request =
  GET    Route (Maybe [Param]) |
  POST   Route (Maybe Body)    |
  PUT    Route (Maybe [Param]) (Maybe Body) |
  DELETE Route (Maybe [Param]) deriving (Show, Eq)

type Param = (String, String)
type Route = String
type Body = String

parseRequest :: [String] -> Maybe Request
parseRequest [] = Nothing
parseRequest (line:lines)
  | method == Just "GET" = parseGetRequest $ split line ' '
  | method == Just "POST" = parsePostRequest (split line ' ') lines
  | method == Just "PUT" = Nothing
  | method == Just "DELETE" = Nothing
  | method == Nothing = parseRequest lines
  | otherwise = parseRequest lines
  where method = getMethod line

getMethod :: String -> Maybe String
getMethod line =
  case split line ' ' of
    [] -> Nothing
    (x:xs) -> Just x

parseGetRequest :: [String] -> Maybe Request
parseGetRequest [] = Nothing
parseGetRequest (_:fullRoute:_) = do
  route <- parseRoute fullRoute
  Just $ GET route $ parseParams fullRoute

parsePostRequest :: [String] -> [String] -> Maybe Request
parsePostRequest (_:fullRoute:_) remaining = do
  route <- parseRoute fullRoute
  Just $ POST route $ parseBody remaining
parsePostRequest _ _ = Nothing

parseRoute :: String -> Maybe Route
parseRoute routeAndParams =
  case split routeAndParams '?' of
    [] -> Nothing
    (('H':'T':'T':'P':_):_) -> Nothing
    (x:_) -> Just $ x

parseParams :: String -> Maybe [Param]
parseParams routeAndParams =
  case split routeAndParams '?' of
    [] -> Nothing
    (_:x:_) -> parseParamStrings (split x '&') []
    _ -> Nothing

parseParamStrings :: [String] -> [Param] -> Maybe [Param]
parseParamStrings [] params = Just params
parseParamStrings (x:xs) parsedParams = do
  param <- parseParamString (split x '=')
  parseParamStrings xs $ parsedParams ++ [param]

parseParamString :: [String] -> Maybe Param
parseParamString [] = Nothing
parseParamString (x:y:[]) = Just $ (x, y)
parseParamString (x:xs) = Nothing

parseBody :: [String] -> Maybe Body
parseBody [] = Nothing
parseBody ("\r":body) = Just $ join body
parseBody (_:remaining) = parseBody remaining