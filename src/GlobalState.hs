module GlobalState where 

    import Error
    import Syntax
    import Utils

    import Control.Monad.Except (ExceptT, runExceptT, throwError)
    import Control.Monad.State

    -- | Monad for handling type errors and labeling.
    type TCheckState a = ExceptT TypeError (State Int) a

    -- | Create a global state for typechecking.
    runTypeChecking :: TCheckState a -> Either TypeError a
    runTypeChecking g = evalState (runExceptT g) 0

    -- | Generate a new label.
    newLabel :: TCheckState Label 
    newLabel = do 
        n <- get 
        put $ n + 1
        return $ Label n

    -- | Monad for handling runtime errors and updating the store environment.
    type EvalState a = ExceptT RuntimeError (State StoreEnv) a

    -- | Create a global state for evaluation.
    runEvaluation :: EvalState a -> Either RuntimeError a
    runEvaluation g = evalState (runExceptT g) emptyStore

    -- | Allocate a new store.
    updateStoreEnv :: Term -> EvalState Term 
    updateStoreEnv t = do 
        storeEnv <- get 
        let (t, newStoreEnv) = allocate storeEnv t
        put newStoreEnv
        return t

    -- | Look up a reference.
    peekStoreEnv :: Int -> EvalState Term
    peekStoreEnv l = do 
        storeEnv <- get 
        case storeEnv `lookUp` l of 
            Just (Store (v, _)) -> return v
            Nothing             -> throwError $ InvalidRef l