module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys 
  | length xs /= length ys = Nothing
  | otherwise              = Just $ length different
  where 
    different = filter (\(x, y) -> x /= y) zipped
    zipped    = zip xs ys
