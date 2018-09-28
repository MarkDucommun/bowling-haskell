module Router (
  route
  , responseWriter
  , RequestHandler(GET)
  , Response(OK, CREATED, BAD_REQUEST, NOT_FOUND)
  , ResponseBody(Empty, Json, Text)
) where

import           RequestParser

data RequestHandler = GET Route (Maybe [Param] -> Response) |
                      POST Route (Maybe Body -> Response) |
                      PUT Route (Maybe [Param] -> Maybe Body -> Response)|
                      DELETE Route (Maybe [Param] -> Response)

data Response = OK ResponseBody | CREATED | BAD_REQUEST ResponseBody | NOT_FOUND

data ResponseBody = Empty | Json String | Text String

route :: [RequestHandler] -> Maybe Request -> Response

route [] _ = NOT_FOUND

route _ Nothing = NOT_FOUND

route ((Router.GET someRoute requestHandler):handlers) request@(Just (RequestParser.GET actualRoute params))
  | someRoute == actualRoute = requestHandler params
  | otherwise = route handlers request

route ((Router.POST someRoute requestHandler):handlers) request@(Just (RequestParser.POST actualRoute body))
  | someRoute == actualRoute = requestHandler body
  | otherwise = route handlers request

route ((Router.PUT someRoute requestHandler):handlers) request@(Just (RequestParser.PUT actualRoute params body))
  | someRoute == actualRoute = requestHandler params body
  | otherwise = route handlers request

route ((Router.DELETE someRoute requestHandler):handlers) request@(Just (RequestParser.DELETE actualRoute params))
  | someRoute == actualRoute = requestHandler params
  | otherwise = route handlers request

route _ _ = NOT_FOUND

responseWriter :: Response -> String

responseWriter (OK Empty) = "HTTP/1.0 204 NO CONTENT\r\n\r\n"

responseWriter (OK (Json body)) = "HTTP/1.0 200 OK\r\nContent-Type: Appliction/JSON\r\nContent-Length: " ++ (show $ length body) ++ "\r\n\r\n" ++ body ++ "\r\n"

responseWriter (OK (Text body)) = "HTTP/1.0 200 OK\r\nContent-Type: text/html\r\nContent-Length: " ++ (show $ length body) ++ "\r\n\r\n" ++ body ++ "\r\n"

responseWriter CREATED = "HTTP/1.0 201 CREATED\r\n\r\n"

responseWriter NOT_FOUND = "HTTP/1.0 404 NOT FOUND\r\n\r\n"

responseWriter (BAD_REQUEST Empty) = "HTTP/1.0 422 BAD REQUEST\r\n\r\n"

responseWriter (BAD_REQUEST (Json body)) = "HTTP/1.0 422 OK\r\nContent-Type: Appliction/JSON\r\nContent-Length: " ++ (show $ length body) ++ "\r\n\r\n" ++ body ++ "\r\n"

responseWriter (BAD_REQUEST (Text body)) = "HTTP/1.0 422 OK\r\nContent-Type: text/html\r\nContent-Length: " ++ (show $ length body) ++ "\r\n\r\n" ++ body ++ "\r\n"