module Main where

import           Control.Exception
import           Server
import           ScoreBowlingGame

main :: IO ()
main = startServer (PortNumber 8080) [scoreBowlingGame, goodbyeName, helloName]

helloName :: RequestHandler
helloName = GET "/hello" $ Impure respondHello
  where
    respondHello Nothing = return $ OK $ Text "<h1 style='color: green'>Hello Nobody</h1>"
    respondHello (Just params) =
      case getParam params "file" of
        Nothing -> return $ OK $ Text "<h1>Hello Nobody</h1>"
        (Just value) -> do
          fileContents <- try $ readFile value :: IO (Either IOError String)
          case fileContents of
            Left _ -> return $ NOT_FOUND
            Right fileStuff -> return $ OK $ Text $ "<h1>Hello " ++ fileStuff ++ "</h1>"

goodbyeName :: RequestHandler
goodbyeName = GET "/goodbye" $ Pure respondGoodbye
  where
    respondGoodbye Nothing = OK $ Text "<h1>Goodbye Nobody</h1>"
    respondGoodbye (Just params) =
      case getParam params "name" of
        Nothing      -> OK $ Text "<h1>Goodbye Nobody</h1>"
        (Just value) -> OK $ Text $ "<h1>Goodbye " ++ value ++ "</h1>"

b :: RequestHandler
b = PUT "/b" handleB
  where
    handleB _ (Just body) = return $ OK $ Text body
    handleB _ _ = return $ OK $ Empty

resetGame :: RequestHandler
resetGame = GET "/reset" $ Impure (\_ -> return CREATED)