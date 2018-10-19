module Server
  ( startServer
  , PortID(PortNumber)
  , RequestHandler(GET, POST, PUT, DELETE)
  , Response(OK, CREATED, BAD_REQUEST, NOT_FOUND)
  , ResponseBody(Empty, Json, Text)
  , ParamHandler(Pure, Impure)
  , ImpureBodyHandler
  , ImpureParamAndBodyHandler
  , ImpureParamHandler
  , getParam
  ) where

import           Control.Concurrent
import           Control.Exception
import           Network
import           RequestParser
import           ResponseWriter
import           Router
import           SocketHandler
import           System.IO

startServer :: PortID -> [RequestHandler] -> IO ()
startServer port handlers =
  withSocketsDo $ do
    sock <- listenOn $ port
    loop sock handlers

loop :: Socket -> [RequestHandler] -> IO ()
loop sock handlers = do
  (h, _, _) <- accept sock
  _ <- forkIO $ body h
  loop sock handlers
  where
    body h = do
      msgLines <- readRequest h `catch` handleNoInput
      response <- handleRequest handlers msgLines `catch` handleProcessingError
      hPutStr h response
      hFlush h
      hClose h

handleRequest :: [RequestHandler] -> [String] -> IO String
handleRequest handlers msgLines = do
  response <- route handlers $ parseRequest msgLines
  return $ responseWriter response

handleNoInput :: IOError -> IO [String]
handleNoInput _ = return []

handleProcessingError :: IOError -> IO String
handleProcessingError _ = return $ responseWriter $ BAD_REQUEST Empty
