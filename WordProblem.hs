module WordProblem (answer) where

import NanoParsec (number, parse, Parser, string)
import Control.Applicative (many, (<$>), (<|>), (*>))

data Term = Add Int
          | Minus Int
          | Multiply Int
          | Divide Int
          deriving (Show)

add, minus, multiply, divide :: Parser Term
add      = Add      <$> (string " plus "          *> number)
minus    = Minus    <$> (string " minus "         *> number)
multiply = Multiply <$> (string " multiplied by " *> number)
divide   = Divide   <$> (string " divided by "    *> number)

term :: Parser Term
term = add <|> minus <|> multiply <|> divide

expression :: Parser [Term]
expression = do
  string "What is "
  start <- number
  terms <- many term
  string "?"
  return $ (Add start) : terms

calculate :: [Term] -> Int
calculate ts = go 0 ts
  where
    go val []     = val
    go val (t:ts) = case t of
                      Add x      -> go (val + x) ts
                      Minus x    -> go (val - x) ts
                      Multiply x -> go (val * x) ts
                      Divide x   -> go (val `div` x) ts

answer :: String -> Maybe Int
answer problem = case parse expression problem of
                   []             -> Nothing
                   [(terms, [])]  -> Just $ calculate terms
