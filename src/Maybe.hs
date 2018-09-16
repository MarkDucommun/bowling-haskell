module Maybe (aybe) where

aybe :: Maybe a -> (a -> Maybe b) -> Maybe b
aybe Nothing _ = Nothing
aybe (Just a) fn = fn a