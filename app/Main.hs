module Main where

import           BowlingLib
import           Control.Concurrent
import           InputLib
import           Network
import           RequestParser
import           Router
import           SocketHandler
import           System.IO

main :: IO ()
main =
  withSocketsDo $ do
    sock <- listenOn $ PortNumber 8080
    loop sock

loop :: Socket -> IO ()
loop sock = do
  (h, _, _) <- accept sock
  _ <- forkIO $ body h
  loop sock
  where
    body h = do
      msgLines <- readRequest h
      putStrLn "REQUEST"
      s <- readFile "hello.txt"
      putStrLn s
      let response = handleRequest msgLines
      hPutStr h response
      hFlush h
      hClose h

requestHandler :: Maybe Request -> Response
requestHandler = route [helloName, goodbyeName, scoreBowlingGame]

handleRequest :: [String] -> String
handleRequest msgLines = responseWriter $ requestHandler $ parseRequest msgLines

helloName :: RequestHandler
helloName = Router.GET "/hello" respondHello
  where
    respondHello Nothing = OK $ Text "<h1 style='color: green'>Hello Nobody</h1>"
    respondHello (Just params) =
      case getParam params "name" of
        Nothing      -> OK $ Text "<h1>Hello Nobody</h1>"
        (Just value) -> OK $ Text $ "<h1>Hello " ++ value ++ "</h1>"

goodbyeName :: RequestHandler
goodbyeName = Router.GET "/goodbye" respondGoodbye
  where
    respondGoodbye Nothing = OK $ Text "<h1>Goodbye Nobody</h1>"
    respondGoodbye (Just params) =
      case getParam params "name" of
        Nothing      -> OK $ Text "<h1>Goodbye Nobody</h1>"
        (Just value) -> OK $ Text $ "<h1>Goodbye " ++ value ++ "</h1>"

scoreBowlingGame :: RequestHandler
scoreBowlingGame = Router.GET "/bowling" calculateScore
  where
    calculateScore Nothing = BAD_REQUEST $ Text "<h1 style='color: red'>No Frames Present</h1>"
    calculateScore (Just params) =
      case getParam params "frames" of
        Nothing -> BAD_REQUEST $ Text "<h1 style='color: red'>No Frames Present</h1>"
        (Just frames) ->
          case parseInput frames of
            Nothing -> BAD_REQUEST $ Text "<h1 style='color: red'>Incorrect Frame Format</h1>"
            (Just frameList) ->
              case score frameList of
                Nothing -> BAD_REQUEST $ Text "<h1 style='color: red'>Incorrect Frame Format</h1>"
                (Just theScore) -> OK $ Text $ "<h1 style='color: green'>Score: " ++ show theScore ++ "</h1>"
