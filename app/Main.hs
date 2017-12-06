module Main where
    
    import Evaluator
    import TypeChecker
    import Parser
    import Prettier
    
    import Control.Monad.Trans
    import System.Console.Haskeline
    
    -- interpret each line input, either printing out the result or error
    interpret :: String -> IO ()
    interpret line = case parseExpr line of 
      Right validExpr -> case typeOf validExpr of 
                           Right _  -> case evaluate validExpr of 
                                        Just res -> putStrLn $ printPretty res
                                        Nothing  -> putStrLn "Cannot evaluate"
                           Left err -> putStrLn err
      Left err        -> print err
                           

    main :: IO ()
    main = runInputT defaultSettings loop
      where
      loop :: InputT IO ()
      loop = do
        input <- getInputLine "> "
        case input of
          Nothing      -> outputStrLn "Goodbye."
          Just validIn -> liftIO (interpret validIn) >> loop
