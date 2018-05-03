module GlobalState where 

    import Error
    import Syntax

    import Control.Monad.Except (ExceptT, runExceptT)
    import Control.Monad.State

    -- | Create a monad for error handling and global state.
    type TCheckState a = ExceptT TypeError (State Int) a

    -- | Create a global state for a function.
    runTypeChecking :: TCheckState a -> (Either TypeError a, Int)
    runTypeChecking g = runState (runExceptT g) 0

    -- | Generate a new label.
    newLabel :: TCheckState Label 
    newLabel = do 
        n <- get 
        put $ n + 1
        return $ Label n