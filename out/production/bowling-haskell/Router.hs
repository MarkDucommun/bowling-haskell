module Router
  ( route
  , RequestHandler(GET, POST, PUT, DELETE)
  , Response(OK, CREATED, BAD_REQUEST, NOT_FOUND)
  , ResponseBody(Empty, Json, Text)
  , ParamHandler(Pure, Impure)
  , ImpureBodyHandler
  , ImpureParamAndBodyHandler
  , ImpureParamHandler
  ) where

import           RequestParser

data RequestHandler
  = GET Route
        ParamHandler
  | POST Route
        ImpureBodyHandler
  | PUT Route
        ImpureParamAndBodyHandler
  | DELETE Route
        ImpureParamHandler

data ParamHandler
  = Pure PureParamHandler
  | Impure ImpureParamHandler

type PureParamHandler = (Maybe [Param] -> Response)

type ImpureParamHandler = (Maybe [Param] -> IO Response)

type ImpureBodyHandler = (Maybe Body -> IO Response)

type ImpureParamAndBodyHandler = (Maybe [Param] -> Maybe Body -> IO Response)

data Response
  = OK ResponseBody
  | CREATED
  | BAD_REQUEST ResponseBody
  | NOT_FOUND

data ResponseBody
  = Empty
  | Json String
  | Text String

route :: [RequestHandler] -> Maybe Request -> IO Response
route [] _ = return NOT_FOUND
route _ Nothing = return NOT_FOUND
route ((Router.GET someRoute (Pure requestHandler)):handlers) request@(Just (RequestParser.GET actualRoute params))
  | someRoute == actualRoute = return $ requestHandler params
  | otherwise = route handlers request
route ((Router.GET someRoute (Impure requestHandler)):handlers) request@(Just (RequestParser.GET actualRoute params))
  | someRoute == actualRoute = requestHandler params
  | otherwise = route handlers request
route ((Router.POST someRoute requestHandler):handlers) request@(Just (RequestParser.POST actualRoute body))
  | someRoute == actualRoute = requestHandler body
  | otherwise = route handlers request
route ((Router.PUT someRoute requestHandler):handlers) request@(Just (RequestParser.PUT actualRoute params body))
  | someRoute == actualRoute = requestHandler params body
  | otherwise = route handlers request
route ((Router.DELETE someRoute requestHandler):handlers) request@(Just (RequestParser.DELETE actualRoute params))
  | someRoute == actualRoute = do
    putStrLn $ "DELETE " ++ someRoute
    requestHandler params
  | otherwise = route handlers request
route _ _ = return NOT_FOUND
