module Main where
    
    import Evaluator
    import Parser
    import Prettier
    import TypeChecker

    import Control.Monad.Trans
    import System.Console.Haskeline

    -- | Interpret a line input, either printing out the result or error.
    interpret :: String -> IO ()
    interpret line = case parseExpr line of 
        Right validExpr -> case typeCheck validExpr of 
                               Right t  -> case evaluate t of 
                                               Right res -> printRes res
                                               Left err  -> printMsg err
                               Left err -> printMsg err
        Left err        -> print err                       

    -- | Run test on a string input.
    test :: String -> IO ()
    test s = do 
        putStr "Test> "
        putStrLn s
        interpret s
        putStrLn "\n" 

    -- | Run all the test cases. 
    runTests :: IO ()
    runTests = do 
        test "(\\m. ((\\x:Nat->Nat. (x 0)) m)) true" 
        test "(\\x. succ x) true"
        test "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. iszero y)"
        test "(\\x:Nat->Nat. x (x 0)) (\\x. (succ x))"
        test "((\\m. if (\\x. iszero x) m then (\\x. succ x) else (\\x. pred x)) 0) true"
        test "(\\x:Nat->Bool. x 0) (\\x:Nat. x)"
        test "(\\n. (\\m. if (\\x. iszero x) n then m 0 else m (succ 0)) (\\y. succ y)) 0"
        test "(\\x. (x 0)) (\\x:Nat. (succ x))"
        test "(\\m. ((\\x:Nat->Bool. (x 0)) m)) (\\y:Nat. succ y)"
        test "(\\m. ((\\x. (x 0)) m)) (\\y:Nat. succ y)"
        test "(\\m. ((\\x. (x (succ (succ 0)))) m)) (\\y:Ref Nat. !y)"
        test "(\\m. ((\\x. (x ref (succ (succ 0)))) m)) (\\y:Ref Nat. !y)"
        test "(\\x. x:=succ 0) ref 0"
        test "(\\x:Ref Bool. !x) ref 0"
        test "(\\m. ((\\x. (x ref (succ (succ 0)))) m)) (\\y:Ref Bool. !y)"
        test "(\\m. ((\\x. (ref (succ (succ 0))) := x) m)) 0"
        test "(\\m. ((\\x. (ref (succ (succ 0))) := x) m)) true"
        test "(\\m. ((\\x. x := succ 0) m)) ref 0"
        test "(\\m. ((\\x. x := true) m)) ref 0"
        test "(\\m. ((\\x. x := succ 0) m)) ref true"
        test "(\\m. ((\\x:Ref Nat. x := succ 0) m)) ref true"
        test "(\\m. ((\\x. !x) m)) ref true"
        test "(\\m. ((\\x. !x) m)) 0"
        test "(\\y. (\\x:Ref Bool. x := 0) ref (iszero y)) 0"

    -- | Print new line.
    newLine :: InputT IO ()
    newLine = lift $ putStr "\n"

    -- | Run a read-eval-print loop.
    loop :: InputT IO ()
    loop = do
        input <- getInputLine "GTLC> "
        case input of
            Just "exit"  -> return ()
            Just "test"  -> lift runTests >> loop
            Just validIn -> lift (interpret validIn) >> newLine >> loop
                            
    -- | Main method for the interpreter.
    main :: IO ()
    main = do 
        putStrLn "Gradually Typed Lambda Calculus, verion 1.0.0: https://github.com/thuytien140894/GTLC"
        runInputT defaultSettings loop