module Main where
    
    import Evaluator
    import TypeChecker
    import Parser
    import Prettier
    
    import Control.Monad.Trans
    import System.Console.Haskeline
    
    -- interpret a line input, either printing out the result or error
    interpret :: String -> IO ()
    interpret line = case parseExpr line of 
      Right validExpr  -> case typeCheck validExpr of 
                            Right t  -> case evaluate t of 
                                          Right res -> printRes res
                                          Left err  -> printMsg err
                            Left err -> printMsg err
      Left err         -> print err                       

    -- print new line
    printNewLine :: IO ()
    printNewLine = putStrLn "\n"

    -- run a read-eval-print loop
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "GTLC> "
      case input of
        Just "exit"  -> return ()
        Just validIn -> lift (interpret validIn) >> lift printNewLine >> loop
                            
    -- main method for the interpreter
    main :: IO ()
    main = do 
      putStrLn "Gradually Typed Lambda Calculus, verion 1.0.0: https://github.com/thuytien140894/GTLC" 
      runInputT defaultSettings loop