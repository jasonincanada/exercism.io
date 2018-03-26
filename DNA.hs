module DNA (toRNA) where

import Control.Monad (liftM2)

toRNA :: String -> Maybe String
toRNA str = foldr (liftM2 (:) . f) init str
  where 
    init  = Just ""

    f :: Char -> Maybe Char
    f 'G' = Just 'C'
    f 'C' = Just 'G'
    f 'T' = Just 'A'
    f 'A' = Just 'U'
    f _   = Nothing

