module Brackets (arePaired) where

isOpening, isClosing :: Char -> Bool
isOpening = flip elem "[{("
isClosing = flip elem ")}]"

openFor :: Char -> Char
openFor ')' = '('
openFor ']' = '['
openFor '}' = '{'

arePaired :: String -> Bool
arePaired = go []
  where 
    go stack []     = null stack
    go stack (c:cs)
      | isOpening c = go (c:stack) cs
      | isClosing c = if null stack
                      then False
                      else if head stack == openFor c
                           then go (tail stack) cs
                           else False
      | otherwise   = go stack cs
                     
