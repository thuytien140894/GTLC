module GlobalState where 

    import Error
    import StoreEnv (StoreEnv, Store(Store))
    import Syntax 

    import qualified StoreEnv

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
    allocate :: Term -> SEvalState Term 
    allocate t = do 
        env <- get 
        let (t', newEnv) = StoreEnv.allocate env t
        put newEnv
        return t'

    -- | Update the value of an existing store.
    update :: Int -> Term -> SEvalState Term
    update l t = do 
        env <- get 
        case env `StoreEnv.lookUp` l of 
            Just (Store (_, s)) -> do let v = Store (t, s)
                                      put $ StoreEnv.insert env l v
                                      return t
            Nothing             -> throwError $ InvalidRef l

    -- | Look up a reference.
    peek :: Int -> SEvalState Term
    peek l = do 
        env <- get 
        case env `StoreEnv.lookUp` l of 
            Just (Store (v, _)) -> return v
            Nothing             -> throwError $ InvalidRef l