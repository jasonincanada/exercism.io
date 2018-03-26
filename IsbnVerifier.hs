module IsbnVerifier (isbn) where

import Data.Char (ord)

-- From: http://exercism.io/exercises/haskell/isbn-verifier/readme
--
-- The first 9 digits in the ISBN have to be between 0 and 9.
-- The check digit can additionally be an 'X' to allow 10 to be a valid check digit as well.
-- (x1 * 10 + x2 * 9 + x3 * 8 + x4 * 7 + x5 * 6 + x6 * 5 + x7 * 4 + x8 * 3 + x9 * 2 + x10 * 1) mod 11 == 0

isbn :: String -> Bool
isbn s = case getTotal (nodashes s) of
           Just v  -> v `mod` 11 == 0
           Nothing -> False
  where nodashes = filter (/='-')
        getTotal s = f s 0 10

-- "3598215088" 0     10
--  "598215088" 30    9
--   "98215088" 30+45 8
--         ...
f :: String -> Int -> Int -> Maybe Int
f []     total n 
  | n > 1        = Nothing    -- not enough characters
  | otherwise    = Just total -- finished the computation
f _      _     0 = Nothing    -- too many characters
f (x:xs) total n = case (toVal x n) of
                     Just v  -> f xs (total+v) (n-1)
                     Nothing -> Nothing

toVal :: Char -> Int -> Maybe Int
toVal c n
  | c `elem` ['0'..'9'] = Just (n * (ord c - ord '0'))
  | c == 'X' && n == 1  = Just 10
  | otherwise           = Nothing


