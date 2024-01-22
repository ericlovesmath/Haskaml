module SubInterpreter (Expr (..), subEval) where

import Parser (Expr (..))

-- Evaluates function
exec :: String -> [Expr] -> Expr
exec "+" [Num x, Num y] = Num (x + y)
exec "*" [Num x, Num y] = Num (x * y)
exec "-" [Num x, Num y] = Num (x - y)
exec "/" [Num x, Num y] = Num (div x y)
exec "=" [Num x, Num y] = Bool (x == y)
exec "+." [Float x, Float y] = Float (x + y)
exec "*." [Float x, Float y] = Float (x * y)
exec "-." [Float x, Float y] = Float (x - y)
exec "/." [Float x, Float y] = Float (x / y)
exec "inc" [Num x] = Num (x + 1)
exec "dec" [Num x] = Num (x - 1)
exec op expr = error $ "Unknown function " ++ op ++ show expr

-- Evaluates an expression
eval :: String -> Expr -> IO (Expr)
eval tab (Float n) = do
    putStrLn $ tab ++ "evaluate: " ++ show n ++ " --> " ++ show n
    return $ Float n
eval tab (Num n) = do
    putStrLn $ tab ++ "evaluate: " ++ show n ++ " --> " ++ show n
    return $ Num n
eval tab (Bool bool) = do
    putStrLn $ tab ++ "evaluate: " ++ show bool ++ " --> " ++ show bool
    return $ Bool bool
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
    bind' <- eval ('\t' : tab) bind
    putStrLn $ tab ++ "substitute: " ++ show (Let sym bind' expr)
    eval tab $ sub sym bind' expr
eval tab (If cond t f) = do
    putStrLn $ "evaluate: " ++ show (If cond t f)
    let tab' = '\t' : tab
    putStrLn $ tab' ++ "if is a special form, so evaluate the first operand:"
    Bool cond' <- eval tab' cond
    next <-
        if (cond')
            then do
                putStrLn $ tab' ++ "first argument of if is true, so evaluate the second operand:"
                return t
            else do
                putStrLn $ tab' ++ "first argument of if is false, so evaluate the third operand:"
                return f
    res <- eval tab' next
    return res
eval _ (Sym sym) = error $ "Sym " ++ sym ++ " failed to be substituted"

-- Subtitutes in variables recursively
-- Avoids shadowing variables
sub :: String -> Expr -> Expr -> Expr
sub sym bind (Fn op expr) = Fn op $ map (sub sym bind) expr
sub sym bind (If cond t f) = If (sub sym bind cond) (sub sym bind t) (sub sym bind f)
sub sym bind (Sym sym') | sym' == sym = bind
sub sym bind (Let sym' bind' expr) = Let sym' (sub sym bind bind') scoped_expr
  where
    scoped_expr = if sym' == sym then expr else (sub sym bind expr)
sub _ _ expr = expr

subEval :: Expr -> IO Expr
subEval = do eval ""
