module GlobalState where 

    import Error
    import StoreEnv
    import Syntax 

    import Control.Monad.Except (ExceptT, runExceptT, throwError)
    import Control.Monad.State

    -- | Monad for handling type errors and labeling.
    type TCheckState a = ExceptT TypeError (State Int) a

    -- | Create a global state for typechecking.
    runTyCheck :: TCheckState a -> Either TypeError a
    runTyCheck g = evalState (runExceptT g) 0

    -- | Generate a new label.
    newLabel :: TCheckState Label 
    newLabel = do 
        n <- get 
        put $ n + 1
        return $ Label n

    -- | Monad for handling runtime errors and updating the store environment.
    type EvalState a = ExceptT RuntimeError (State StoreEnv) a

    -- | Create a global state for small-step evaluation.
    runEval' :: EvalState a -> StoreEnv -> (Either RuntimeError a, StoreEnv)
    runEval' g = runState (runExceptT g)

    -- | Unwrap monads for big-step evaluation.
    runEval :: EvalState a -> Either RuntimeError a
    runEval g = evalState (runExceptT g) emptyStore

    -- | Allocate a new store.
    allocateStoreEnv :: Term -> EvalState Term 
    allocateStoreEnv t = do 
        storeEnv <- get 
        let (t', newStoreEnv) = allocate storeEnv t
        put newStoreEnv
        return t'

    -- | Update the value of an existing store.
    updateStoreEnv :: Int -> Term -> EvalState Term
    updateStoreEnv l t = do 
        storeEnv <- get 
        case storeEnv `lookUp` l of 
            Just (Store (_, s)) -> do let v = Store (t, s)
                                      put $ insertRef storeEnv l v
                                      return t
            Nothing             -> throwError $ InvalidRef l

    -- | Look up a reference.
    peekStoreEnv :: Int -> EvalState Term
    peekStoreEnv l = do 
        storeEnv <- get 
        case storeEnv `lookUp` l of 
            Just (Store (v, _)) -> return v
            Nothing             -> throwError $ InvalidRef l