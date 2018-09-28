module RequestParser (
  Request (GET, POST, PUT, DELETE)
  , Param
  , Route
  , Body
  , parseRequest
  , getParam
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

getParam :: [Param] -> String -> Maybe String
getParam [] _ = Nothing
getParam ((key, value):xs) param
  | key == param = Just $ value
  | otherwise = getParam xs param

parseRequest :: [String] -> Maybe Request
parseRequest [] = Nothing
parseRequest (line:theLines)
  | method == Just "GET" = parseGetRequest $ split line ' '
  | method == Just "POST" = parsePostRequest (split line ' ') theLines
  | method == Just "PUT" = parsePutRequest (split line ' ') theLines
  | method == Just "DELETE" = parseDeleteRequest $ split line ' '
  | method == Nothing = parseRequest theLines
  | otherwise = parseRequest theLines
  where method = getMethod line

getMethod :: String -> Maybe String
getMethod line =
  case split line ' ' of
    [] -> Nothing
    (x:_) -> Just x

parseGetRequest :: [String] -> Maybe Request
parseGetRequest [] = Nothing
parseGetRequest (_:fullRoute:_) = do
  route <- parseRoute fullRoute
  Just $ GET route $ parseParams fullRoute
parseGetRequest _ = Nothing

parsePostRequest :: [String] -> [String] -> Maybe Request
parsePostRequest (_:fullRoute:_) remaining = do
  route <- parseRoute fullRoute
  Just $ POST route $ parseBody remaining
parsePostRequest _ _ = Nothing

parsePutRequest :: [String] -> [String] -> Maybe Request
parsePutRequest (_:fullRoute:_) remaining = do
  route <- parseRoute fullRoute
  Just $ PUT route (parseParams fullRoute) $ parseBody remaining
parsePutRequest _ _ = Nothing

parseDeleteRequest :: [String] -> Maybe Request
parseDeleteRequest [] = Nothing
parseDeleteRequest (_:fullRoute:_) = do
  route <- parseRoute fullRoute
  Just $ DELETE route $ parseParams fullRoute
parseDeleteRequest _ = Nothing

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
parseParamString _ = Nothing

parseBody :: [String] -> Maybe Body
parseBody [] = Nothing
parseBody ("\r":body) = Just $ join body
parseBody (_:remaining) = parseBody remaining