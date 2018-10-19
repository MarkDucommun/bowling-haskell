module ResponseWriter where

import Router

responseWriter :: Response -> String

responseWriter (OK Empty) =
  "HTTP/1.0 204 NO CONTENT\r\n\r\n"

responseWriter (OK (Json body)) =
  "HTTP/1.0 200 OK\r\nContent-Type: Appliction/JSON\r\nContent-Length: " ++ (show $ length body) ++ "\r\n\r\n" ++ body ++ "\r\n"

responseWriter (OK (Text body)) =
  "HTTP/1.0 200 OK\r\nContent-Type: text/html\r\nContent-Length: " ++ (show $ length body) ++ "\r\n\r\n" ++ body ++ "\r\n"

responseWriter CREATED =
  "HTTP/1.0 201 CREATED\r\n\r\n"

responseWriter NOT_FOUND =
  "HTTP/1.0 404 NOT FOUND\r\n\r\n"

responseWriter (BAD_REQUEST Empty) =
  "HTTP/1.0 422 BAD REQUEST\r\n\r\n"

responseWriter (BAD_REQUEST (Json body)) =
  "HTTP/1.0 422 BAD REQUEST\r\nContent-Type: Appliction/JSON\r\nContent-Length: " ++ (show $ length body) ++ "\r\n\r\n" ++ body ++ "\r\n"

responseWriter (BAD_REQUEST (Text body)) =
  "HTTP/1.0 422 BAD REQUEST\r\nContent-Type: text/html\r\nContent-Length: " ++ (show $ length body) ++ "\r\n\r\n" ++ body ++ "\r\n"