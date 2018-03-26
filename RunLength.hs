module RunLength (decode, encode) where

import NanoParsec
import Data.List (group)
import Control.Applicative (many, (<|>), (<$>), (<*>))

-- A character repeated n number of times
data RunLength = RunLength Int Char

instance Show RunLength where
  show (RunLength n c) = replicate n c

enc :: RunLength -> String
enc (RunLength n c)
  | n == 1    = [c]
  | otherwise = show n ++ [c]

chars :: Parser Char
chars = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ " ")

singleChar :: Parser RunLength
singleChar = RunLength 1 <$> chars

runLength :: Parser RunLength
runLength = RunLength <$> number <*> chars

decoder :: Parser [RunLength]
decoder = many (runLength <|> singleChar)

decode :: String -> String
decode enc = concatMap show $ run decoder enc

encode :: String -> String
encode text = concatMap enc 
                $ map (\s -> RunLength (length s) (head s))
                $ group text

