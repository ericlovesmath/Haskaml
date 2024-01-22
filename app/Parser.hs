module Parser (parseOCaml, Expr (..)) where

import Text.Parsec
import Text.Parsec.String (Parser)

-- TODO: Make Fn support higher ordered functions by mapping from Expr to Func instead
data Expr = Fn String [Expr] | Num Int | Sym String | Bool Bool | Let String Expr Expr | If Expr Expr Expr | Float Float

instance Show Expr where
    show (Num n) = show n
    show (Float n) = show n
    show (Bool bool) = show bool
    show (Fn op [l, r]) | isInfix op = "(" ++ unwords [show l, op, show r] ++ ")"
      where
        isInfix = flip elem $ ["+", "-", "/", "*", "=", "+.", "-.", "/.", "*."]
    show (Fn op args) = "(" ++ unwords (op : map show args) ++ ")"
    show (Sym sym) = sym
    show (Let sym bind expr) = sym ++ " = " ++ show bind ++ " to " ++ show expr
    show (If cond t f) = "if " ++ show cond ++ " then " ++ show t ++ " else " ++ show f

-- I would like to apologize in advance for the parser code

strip :: Parser a -> Parser a
strip p = spaces *> p <* spaces

-- Parses to Num (Int), Sym (String), Bool (Bool)
parseLiteral :: Parser Expr
parseLiteral =
    Num . read <$> many1 digit
        <|> Bool False <$ string "false"
        <|> Bool True <$ string "true"
        <|> Sym <$> many1 letter

-- Parsing Arithmetic Operations, follows order of operations
parseMath :: Parser Expr
parseMath = parseMath0
  where
    parseMath0 = chainl1 parseMath1 (parseOps '+' '-')
    parseMath1 = chainl1 parseMath2 (parseOps '*' '/')
    parseMath2 = strip $ try (between (char '(') (char ')') parseMath0) <|> parseLiteral
    parseOps a b = do
        op <- strip (char a <|> char b)
        return $ \x y -> Fn [op] [x, y]

parseLet :: Parser Expr
parseLet = do
    _ <- strip (string "let")
    name <- many1 letter
    _ <- strip (char '=')
    bind <- parseMath
    _ <- strip (string "in")
    expr <- parseLet <|> parseMath
    return $ Let name bind expr

parseOCaml :: String -> Either ParseError Expr
parseOCaml = parse (strip (try parseLet <|> parseMath)) ""
