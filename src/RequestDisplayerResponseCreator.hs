module RequestDisplayerResponseCreator(
  createRequestDisplayResponse
) where

import RequestParser

createRequestDisplayResponse :: Maybe Request -> String

createRequestDisplayResponse Nothing = "HTTP/1.0 422 BAD REQUEST\r\nContent-Length: 17\r\n\r\nMalformed Request\r\n"

createRequestDisplayResponse (Just (GET route (Just params))) =
  let body = "GET " ++ route ++ "\r\n" ++ (show params) ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createRequestDisplayResponse (Just (GET route Nothing)) =
  let body = "GET " ++ route ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createRequestDisplayResponse (Just (POST route (Just body))) =
  let responseBody = "POST " ++ route ++ "\r\n" ++ show body ++ "\r\n" in
  msgFront ++ (show $ length responseBody) ++ msgMiddle ++ responseBody ++ msgBack

createRequestDisplayResponse (Just (POST route Nothing)) =
  let body = "POST " ++ route ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createRequestDisplayResponse (Just (PUT route (Just params) (Just body))) =
  let responseBody = "PUT " ++ route ++ "\r\n" ++ (show params) ++ "\r\n" ++ show body  ++ "\r\n" in
  msgFront ++ (show $ length responseBody) ++ msgMiddle ++ responseBody ++ msgBack

createRequestDisplayResponse (Just (PUT route (Just params) Nothing)) =
  let body = "PUT " ++ route ++ "\r\n" ++ (show params) ++ "\r\n"  in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createRequestDisplayResponse (Just (PUT route Nothing (Just body))) =
  let responseBody = "PUT " ++ route ++ "\r\n" ++ show body  ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ responseBody ++ msgBack

createRequestDisplayResponse (Just (PUT route Nothing Nothing)) =
  let body = "PUT " ++ route ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createRequestDisplayResponse (Just (DELETE route (Just params))) =
  let body = "DELETE " ++ route ++ "\r\n" ++ (show params) ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

createRequestDisplayResponse (Just (DELETE route Nothing)) =
  let body = "DELETE " ++ route ++ "\r\n" in
  msgFront ++ (show $ length body) ++ msgMiddle ++ body ++ msgBack

msgFront :: String
msgFront = "HTTP/1.0 200 OK\r\nContent-Length: "

msgMiddle :: String
msgMiddle = "\r\n\r\n"

msgBack :: String
msgBack = "\r\n"
