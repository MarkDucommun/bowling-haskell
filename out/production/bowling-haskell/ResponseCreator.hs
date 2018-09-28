module ResponseCreator(
  createResponse
) where

import RequestParser

createResponse :: Maybe Request -> String

createResponse Nothing = "HTTP/1.0 422 BAD REQUEST\r\nContent-Length: 17\r\n\r\nMalformed Request\r\n"

createResponse (Just (GET route (Just params))) =
  let body = "GET " ++ route ++ "\r\n" ++ (show params) ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createResponse (Just (GET route Nothing)) =
  let body = "GET " ++ route ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createResponse (Just (POST route (Just body))) =
  let responseBody = "POST " ++ route ++ "\r\n" ++ show body in
  msgFront ++ (show $ length responseBody) ++ msgMiddle ++ responseBody ++ msgBack

createResponse (Just (POST route Nothing)) =
  let body = "GET " ++ route ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createResponse _ = "HTTP/1.0 422 BAD REQUEST\r\nContent-Length: 17\r\n\r\nMalformed Request\r\n"

msgFront = "HTTP/1.0 200 OK\r\nContent-Length: "
msgMiddle = "\r\n\r\n"
msgBack = "\r\n"
