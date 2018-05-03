module Evaluator 
    ( evaluate
    ) where

    import Coercion 
    import Error
    import GlobalState
    import StoreEnv
    import Syntax
    import Utils
    
    import Control.Monad.Except 
    import Control.Monad.State 
    import Data.Either
    import Data.Maybe (fromJust)

    -- | Determine if a term is a value.
    isVal :: Term -> Bool
    isVal t = isUncoercedVal t || isCoercedVal t
    
    -- | Determine if a term is a numeric value.
    isNumeric :: Term -> Bool
    isNumeric t = case t of 
        Zero    -> True
        Succ t' -> isNumeric t'
        _       -> False

    -- | Determine if a list contains all values.
    areAllVal :: [Entry] -> Bool
    areAllVal []    = True
    areAllVal ((l1, t1) : ys)  
        | isVal t1  = areAllVal ys
        | otherwise = False

    -- | Determine if a term is an uncoerced value.
    isUncoercedVal :: Term -> Bool
    isUncoercedVal t = case t of 
        Unit              -> True
        Tru               -> True
        Fls               -> True
        t' | isNumeric t' -> True
        Lambda{}          -> True
        Rec ls            -> areAllVal ls
        Loc _             -> True
        _                 -> False

    -- | Determine if a term is a coerced value.
    isCoercedVal :: Term -> Bool
    isCoercedVal (Cast c v)  
        | isUncoercedVal v && isRegular c = True 
    isCoercedVal _                        = False
    
    -- | Perform substitution from the beginning.
    subsFromTop :: Term -> Term -> Term
    subsFromTop s t = shift 0 (-1) (subs 0 (shift 0 1 s) t)

    -- -- | Get the value for the specified field.
    -- getVal :: Term -> String -> Either RuntimeError Term
    -- getVal (Rec []) _ = Left Stuck
    -- getVal (Rec ((l1, t1) : ys)) l 
    --     | l1 == l     = Right t1
    --     | otherwise   = Rec ys `getVal` l

    -- -- | Evaluate a record.
    -- evalRecord :: (Term, StoreEnv) -> Either RuntimeError (Term, StoreEnv) 
    -- evalRecord (Rec [], store) = Right (Rec [], store)
    -- evalRecord (Rec ((l1, t1) : ys), store) 
    --     | isVal t1             = do (rd, store') <- evalRecord (Rec ys, store)
    --                                 Right (rd `addEntry` (l1, t1), store')
    --     | otherwise            = do (t1', store') <- evaluate' (t1, store) 
    --                                 (rd, store'') <- evalRecord (Rec ys, store')
    --                                 Right (rd `addEntry` (l1, t1'), store'')

    -- | Remove an enclosing coercion from a value 
    -- if the run-time type matches the target type.
    unbox :: Term -> EvalState Term
    unbox (Cast c v) = do 
        storeEnv <- get
        case typeOf v storeEnv of 
            srcTy 
                | srcTy `isConsistent` cstTy  -> return v
                | otherwise                   -> let bres = BlameRes None v
                                                 in throwError $ CastError srcTy cstTy bres
              where 
                cstTy = snd $ getCoercionTypes c

    -- | Small-step evaluation.
    evaluate' :: Term -> EvalState Term
    evaluate' t = case t of
        -- | Arithmetic
        Pred Zero                         -> return Zero                         
        Pred (Succ nv) 
            | isNumeric nv                -> return nv                       
        Pred t'                           -> Pred <$> evaluate' t'
        IsZero Zero                       -> return Tru                        
        IsZero (Succ nv) 
            | isNumeric nv                -> return Fls                     
        IsZero t'                         -> IsZero <$> evaluate' t'
        Succ t'                           -> Succ <$> evaluate' t'

        -- | Conditional
        If Tru t2 t3                      -> return t2                        
        If Fls t2 t3                      -> return t3                         
        If t1 t2 t3                       -> (\t1' -> If t1' t2 t3) <$> evaluate' t1

        -- | Cast
        Cast c t' 
            | not (isVal t')              -> Cast c <$> evaluate' t'
        Cast c (Cast d u)                 -> return $ Seq d c `Cast` u            
        Cast (Iden _) u                   -> return u                        
        Cast (Fail s1 s2 l) u             -> throwError $ Blame s1 s2 l Unit                 
        Cast c u 
            | isNormalized c              -> unbox t                              
        Cast c u                          -> return $ reduceCoercion c `Cast` u
        Deref (Cast (CRef c d) (Loc l))   -> return $ Cast d $ Deref (Loc l)      
        Assign (Cast (CRef c d) (Loc l)) v                                                
            | isVal v                     -> return $ Cast d $ Loc l `Assign` Cast c v 
        App (Cast (Func c d) u) v 
            | isUncoercedVal u && isVal v -> return $ Cast d $ u `App` Cast c v                        

        -- | Reference
        Ref v     
            | isVal v                     -> allocateStoreEnv v                             
        Ref t'                            -> Ref <$> evaluate' t'
        Deref (Loc l)                     -> peekStoreEnv l

        -- | Dereference
        Deref t'                          -> Deref <$> evaluate' t'
                                            
        -- | Assignment
        Assign (Loc l) v
            | isVal v                     -> updateStoreEnv l v                       
        Assign v1 t2 
            | isVal v1                    -> Assign v1 <$> evaluate' t2
        Assign t1 t2                      -> (`Assign` t2) <$> evaluate' t1

        -- | Application
        App (Lambda _ t1 _) v2 
            | isVal v2                    -> return $ subsFromTop v2 t1          
        App v1 t2 
            | isVal v1                    -> App v1 <$> evaluate' t2
        App t1 t2                         -> (`App` t2) <$> evaluate' t1             

        -- -- | Records
        -- Proj (Rec ls) l 
        --     | isVal $ Rec ls              -> do t' <- Rec ls `getVal` l                 
        --                                         Right (t', store)
        -- Proj (Rec ls) l                   -> do (t', store') <- evaluate' (Rec ls, store)
        --                                         Right (Proj t' l, store')            
        -- Rec ls 
        --     | not $ isVal $ Rec ls        -> evalRecord (t, store)                      
                        
        -- | No rules applied
        _                                 -> throwError Stuck                                  

    -- | Big-step evaluation
    -- (apply evaluate' repeatedly until a value is reached or we're left with 
    -- an expression that cannot be evaluated further).
    evaluateToValue :: Term -> StoreEnv -> EvalState Term
    evaluateToValue t storeEnv = do 
        let (res, newStoreEnv) = runEval' (evaluate' t) storeEnv
        case res of 
            Right t'               -> evaluateToValue t' newStoreEnv
            Left Stuck             -> return t     
            Left (Blame s1 s2 l _) -> throwError $ Blame s1 s2 l t    
            Left err               -> throwError err
    
    -- | Evaluate a term.
    evaluate :: Term -> Either RuntimeError Term
    evaluate t = case runEval $ evaluateToValue t emptyStore of
        Right t 
            | isUncoercedVal t  -> Right t
            | otherwise         -> Left Stuck 
        Left (Blame s1 s2 l t') -> let cause = fromJust $ blame l t
                                       bres  = BlameRes cause t'
                                   in Left $ CastError s1 s2 bres  
        Left err                -> Left err 