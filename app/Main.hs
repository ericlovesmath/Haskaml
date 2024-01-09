module Main where

-- Note: In reality, instead of substituting, I should be using environments
-- This was huilt for the beginning of CS 4, so it is not true to how OCaml runs
-- Very unfortunate :(
-- Very much has shadowing and scoping issues due to this

data Expr = Fn String [Expr] | Num Int | Sym String | Let String Expr Expr

instance Show Expr where
    show (Num n) = show n
    show (Fn op [l, r]) | isInfix op = "(" ++ unwords [show l, op, show r] ++ ")"
      where
        isInfix = flip elem $ ["+", "-", "/", "*"]
    show (Fn op args) = "(" ++ unwords (op : map show args) ++ ")"
    show (Sym sym) = sym
    show (Let sym bind expr) = sym ++ " = " ++ show bind ++ " to " ++ show expr

-- Evaluates function
exec :: String -> [Expr] -> Expr
exec "+" [Num x, Num y] = Num (x + y)
exec "*" [Num x, Num y] = Num (x * y)
exec "-" [Num x, Num y] = Num (x - y)
exec "/" [Num x, Num y] = Num (div x y)
exec op expr = error $ "Unknown function " ++ op ++ show expr

-- Evaluates an expression
eval :: String -> Expr -> IO (Expr)
eval tab (Num n) = do
    putStrLn $ tab ++ "evaluate: " ++ show n ++ " --> " ++ show n
    return $ Num n
eval tab (Fn op args) = do
    putStrLn $ tab ++ "evaluate: " ++ show (Fn op args)
    let tab' = '\t' : tab
    evalArgs <- mapM (eval tab') args
    let res = exec op evalArgs
    putStrLn $ tab' ++ op ++ " --> [primitive function " ++ op ++ "]"
    putStrLn $ tab' ++ "apply " ++ op ++ " to " ++ show evalArgs ++ " --> " ++ show res
    return res
eval tab (Let sym bind args) = do
    putStrLn $ tab ++ "substitute: " ++ show (Let sym bind args)
    eval tab (sub sym bind args)
eval _ (Sym sym) = error $ "Sym " ++ sym ++ " failed to be substituted"

-- Subtitutes in variables recursively
sub :: String -> Expr -> Expr -> Expr
sub sym bind (Fn op args) = Fn op (map (sub sym bind) args)
sub sym bind (Let sym' bind' args) = Let sym' bind' (sub sym bind args)
sub sym bind (Sym sym')
    | sym' == sym = bind
    | otherwise = Sym sym'
sub _ _ expr = expr

main :: IO ()
main = do
    a <- eval "" $ Fn "+" [Fn "*" [Num 2, Num 5], Num 5]
    putStrLn $ "Final: " ++ show a ++ "\n"
    b <- eval "" $ Let "x" (Num 10) (Fn "*" [Sym "x", Num 5])
    putStrLn $ "Final: " ++ show b ++ "\n"
