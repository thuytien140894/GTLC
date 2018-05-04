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

    -- | Monad for handling runtime errors and updating 
    -- the store environment during small-step evaluation.
    type SEvalState a = ExceptT RuntimeError (State StoreEnv) a

    -- | Create a global state for small-step evaluation.
    runSEval :: SEvalState a -> StoreEnv -> (Either RuntimeError a, StoreEnv)
    runSEval g = runState (runExceptT g)

    -- | Monad for handling runtime errors and updating
    -- the most recently evaluated expression during big-step 
    -- evaluation.
    type BEvalState a = ExceptT RuntimeError (State Term) a

    -- | Unwrap monads for big-step evaluation.
    runBEval :: BEvalState a -> (Either RuntimeError a, Term)
    runBEval g = runState (runExceptT g) Unit

    -- | Allocate a new store.
    allocateStoreEnv :: Term -> SEvalState Term 
    allocateStoreEnv t = do 
        storeEnv <- get 
        let (t', newStoreEnv) = allocate storeEnv t
        put newStoreEnv
        return t'

    -- | Update the value of an existing store.
    updateStoreEnv :: Int -> Term -> SEvalState Term
    updateStoreEnv l t = do 
        storeEnv <- get 
        case storeEnv `lookUp` l of 
            Just (Store (_, s)) -> do let v = Store (t, s)
                                      put $ insertRef storeEnv l v
                                      return t
            Nothing             -> throwError $ InvalidRef l

    -- | Look up a reference.
    peekStoreEnv :: Int -> SEvalState Term
    peekStoreEnv l = do 
        storeEnv <- get 
        case storeEnv `lookUp` l of 
            Just (Store (v, _)) -> return v
            Nothing             -> throwError $ InvalidRef l