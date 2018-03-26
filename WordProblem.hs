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
calculate = foldl f 0
  where f val term = case term of
                       Add x      -> val + x
                       Minus x    -> val - x
                       Multiply x -> val * x
                       Divide x   -> val `div` x

answer :: String -> Maybe Int
answer problem = case parse expression problem of
                   []             -> Nothing
                   [(terms, [])]  -> Just $ calculate terms
