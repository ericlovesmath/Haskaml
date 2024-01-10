module Main where

import Parser (parseOCaml)
import SubInterpreter

printEval :: Expr -> IO ()
printEval expr = do
    val <- subEval expr
    putStrLn $ "Final Expression: " ++ show val ++ "\n"

testParser :: String -> String -> IO ()
testParser inp expect =
    case parseOCaml inp of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right expr ->
            if show expr == expect
                then putStrLn $ "Parsed: " ++ show expr
                else putStrLn $ "Parsed: " ++ show expr ++ "\nEXPECTED: " ++ expect

printParser :: String -> IO ()
printParser inp =
    case parseOCaml inp of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right expr -> printEval expr

main :: IO ()
main = do
    printEval $ Fn "+" [Fn "*" [Num 2, Num 5], Num 5]
    printEval $ Let "x" (Num 10) (Fn "*" [Sym "x", Num 5])

    testParser "3+4*2" "(3 + (4 * 2))"
    testParser "3+4-2+3" "(((3 + 4) - 2) + 3)"
    testParser "3-4+2/3" "((3 - 4) + (2 / 3))"
    testParser " (( 3 - ( 4 + 2 / 3 ) )) " "(3 - (4 + (2 / 3)))"
    testParser " left + 13 * right " "(left + (13 * right))"

    testParser "let x = 10 in x + 1" "x = 10 to (x + 1)"
    testParser "let y = 5 in let x = 10 in 8 - x / y" "y = 5 to x = 10 to (8 - (x / y))"

    printParser "let x = 10 in x + 1"
    printParser "let x = 10 in 8 - x / 2"
    printParser "let y = 5 in let x = 10 in 8 - x / y"
    printParser "let x = 3 in let x = 2 * x in x / 6"

    printParser "let x = 3 in let x = 2 * x in true"
