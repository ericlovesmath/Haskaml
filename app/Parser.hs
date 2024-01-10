module Parser (parseOCaml, Expr (..)) where

import Text.Parsec
import Text.Parsec.String (Parser)

data Expr = Fn String [Expr] | Num Int | Sym String | Let String Expr Expr

instance Show Expr where
    show (Num n) = show n
    show (Fn op [l, r]) | isInfix op = "(" ++ unwords [show l, op, show r] ++ ")"
      where
        isInfix = flip elem $ ["+", "-", "/", "*"]
    show (Fn op args) = "(" ++ unwords (op : map show args) ++ ")"
    show (Sym sym) = sym
    show (Let sym bind expr) = sym ++ " = " ++ show bind ++ " to " ++ show expr

-- I would like to apologize in advance for the parser code

strip :: Parser a -> Parser a
strip p = spaces *> p <* spaces

-- Parses to Num (Int) or Sym (String)
parseLiteral :: Parser Expr
parseLiteral = strip (Num . read <$> many1 digit <|> Sym <$> many1 letter)

-- Parsing Arithmetic Operations, follows order of operations
parseMath :: Parser Expr
parseMath = parseMath0
  where
    parseMath0 = chainl1 parseMath1 (parseOps '+' '-')
    parseMath1 = chainl1 parseMath2 (parseOps '*' '/')
    parseMath2 = try (strip $ between (char '(') (char ')') parseMath0) <|> parseLiteral
    parseOps a b = do
        op <- strip (char a <|> char b)
        return $ \x y -> Fn [op] [x, y]

parseOCaml :: String -> Either ParseError Expr
parseOCaml = parse parseMath ""
