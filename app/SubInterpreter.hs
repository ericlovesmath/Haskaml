module SubInterpreter (Expr (..), subEval) where

import Parser (Expr (..))

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
eval tab (Let sym bind expr) = do
    putStrLn $ tab ++ "substitute: " ++ show (Let sym bind expr)
    eval tab $ sub sym bind expr
eval _ (Sym sym) = error $ "Sym " ++ sym ++ " failed to be substituted"

-- Subtitutes in variables recursively
-- Avoids shadowing variables
sub :: String -> Expr -> Expr -> Expr
sub sym bind (Fn op expr) = Fn op $ map (sub sym bind) expr
sub sym bind (Sym sym') | sym' == sym = bind
sub sym bind (Let sym' bind' expr) = Let sym' (sub sym bind bind') scoped_expr
  where
    scoped_expr = if sym' == sym then expr else (sub sym bind expr)
sub _ _ expr = expr

subEval :: Expr -> IO Expr
subEval = do eval ""
