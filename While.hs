module While where

import Yoda
import Data.Foldable

data Aexp
  = Num Int
  | Var Var
  | Aexp :+: Aexp
  | Aexp :*: Aexp
  | Aexp :-: Aexp
  deriving Show

data Bexp
  = T
  | F
  | Aexp :=: Aexp
  | Aexp :<=: Aexp
  | Bexp :&&: Bexp
  | Not Bexp
  deriving Show

type Var = String

data Stm
  = Var := Aexp
  | Skip
  | Stm :> Stm
  | If Bexp Stm Stm
  | While Bexp Stm
  deriving Show



-- The code below implements left-associative operations in decreasing
-- precedence. It is superceded by the `precedence` function below.


aexp, aexp', aexp'', aexp''' :: Parser Aexp
aexp    = chainl aexp'   ((:*:) <$ tok "*")
aexp'   = chainl aexp''  ((:+:) <$ tok "+")
aexp''  = chainl aexp''' ((:-:) <$ tok "-")
aexp''' = Num <$> num
     <|> Var <$> var
     <|> tok "(" *> aexp <* tok ")"


precedence :: [[Parser (a -> a -> a)]] -> Parser a -> Parser a
precedence ops arg = Data.Foldable.foldl build arg ops
  where build term ops = chainl term (asum ops)

{-aexp = precedence [[(:*:) <$ tok "*"]
                  ,[(:+:) <$ tok "+", (:-:) <$ tok "-"
                  ]]
     $ Num <$> num
   <|> Var <$> var
   <|> tok "(" *> aexp <* tok ")"
   -}

bexp :: Parser Bexp
bexp = precedence [[(:&&:) <$ tok "&"]]
      $ T <$ tok "true"
    <|> F <$ tok "false"
    <|> (:=:) <$> aexp <* tok "=" <*> aexp
    <|> (:<=:) <$> aexp <* tok "<=" <*> aexp
    <|> Not <$ tok "!" <*> bexp
    <|> tok "(" *> bexp <* tok ")"

stms :: Parser Stm
stms = chainl stm ((:>) <$ tok ";")

stm = (:=) <$> var <* tok ":=" <*> aexp
   <|> Skip <$  tok "skip"
   <|> If <$ tok "if" <*> bexp <* tok "then" <*> stm <* tok "else" <*> stm
   <|> While <$ tok "while" <*> bexp <* tok "do" <*> stm
   <|> tok "(" *> stms <* tok ")"


chainl p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest (f x y)
       <|> return x

num :: Parser Int
num = read <$> some (oneOf ['0' .. '9']) <* whitespace

var :: Parser String
var = some (oneOf ['a' .. 'z']) <* whitespace

whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t\n\r")

tok :: String -> Parser String
tok t = string t <* whitespace
