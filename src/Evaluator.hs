module Evaluator 
    ( evaluate
    ) where

    import Coercion 
    import Error
    import Syntax
    import Utils
    
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
        Lambda {}         -> True
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

    -- | Get the value for the specified field.
    getVal :: Term -> String -> Either RuntimeError Term
    getVal (Rec []) _ = Left Stuck
    getVal (Rec ((l1, t1) : ys)) l 
        | l1 == l     = Right t1
        | otherwise   = Rec ys `getVal` l

    -- | Evaluate a record.
    evalRecord :: (Term, StoreEnv) -> Either RuntimeError (Term, StoreEnv) 
    evalRecord (Rec [], store) = Right (Rec [], store)
    evalRecord (Rec ((l1, t1) : ys), store) 
        | isVal t1             = do (rd, store') <- evalRecord (Rec ys, store)
                                    Right (rd `addEntry` (l1, t1), store')
        | otherwise            = do (t1', store') <- evaluate' (t1, store) 
                                    (rd, store'') <- evalRecord (Rec ys, store')
                                    Right (rd `addEntry` (l1, t1'), store'')

    -- | Remove an enclosing coercion from a value 
    -- if the run-time type matches the target type.
    unbox :: Term -> StoreEnv -> Either RuntimeError Term
    unbox (Cast c v) store = case typeOf v store of 
        srcTy 
            | srcTy `isConsistent` cstTy  -> Right v
            | otherwise                   -> let bres  = BlameRes None v
                                             in Left $ CastError srcTy cstTy bres
          where 
            cstTy = snd $ getCoercionTypes c

    -- | Small-step evaluation.
    evaluate' :: (Term, StoreEnv) -> Either RuntimeError (Term, StoreEnv)
    evaluate' (t, store) = case t of
        -- | Arithmetic
        Pred Zero                         -> Right (Zero, store)                         
        Pred (Succ nv) 
            | isNumeric nv                -> Right (nv, store)                           
        Pred t'                           -> do (t'', store') <- evaluate' (t', store)
                                                Right (Pred t'', store')
        IsZero Zero                       -> Right (Tru, store)                          
        IsZero (Succ nv) 
            | isNumeric nv                -> Right (Fls, store)                         
        IsZero t'                         -> do (t'', store') <- evaluate' (t', store)
                                                Right (IsZero t'', store')
        Succ t'                           -> do (t'', store') <- evaluate' (t', store)
                                                Right (Succ t'', store')

        -- | Conditional
        If Tru t2 t3                      -> Right (t2, store)                           
        If Fls t2 t3                      -> Right (t3, store)                           
        If t1 t2 t3                       -> do (t1', store') <- evaluate' (t1, store)
                                                Right (If t1' t2 t3, store')

        -- | Cast
        Cast c t' 
            | not (isVal t')              -> do (t'', store') <- evaluate' (t', store)
                                                Right (Cast c t'', store')
        Cast c (Cast d u)                 -> let t' = Seq d c `Cast` u                   
                                             in Right (t', store)             
        Cast (Iden _) u                   -> Right (u, store)                           
        Cast (Fail s1 s2 l) u             -> Left $ Blame s1 s2 l Unit                 
        Cast c u 
            | isNormalized c              -> do t' <- unbox t store
                                                Right (t', store)                               
        Cast c u                          -> let t' = reduceCoercion c `Cast` u          
                                             in Right (t', store)    
        Deref (Cast (CRef c d) (Loc l))   -> let t' = Cast d $ Deref (Loc l)             
                                             in Right (t', store)       
        Assign (Cast (CRef c d) (Loc l)) v                                                
            | isVal v                     -> let t' = Cast d $ Loc l `Assign` Cast c v
                                             in Right (t', store)    
        App (Cast (Func c d) u) v 
            | isUncoercedVal u && isVal v -> let t' = Cast d $ u `App` Cast c v          
                                             in Right (t', store)                        

        -- | Reference
        Ref v     
            | isVal v                     -> Right $ allocate store v                                   
        Ref t'                            -> do (t'', store') <- evaluate' (t', store)
                                                Right (Ref t'', store')
        Deref (Loc l)                     -> case store `lookUp` l of                      
                                                 Just (Store (v, _)) -> Right (v, store)
                                                 Nothing             -> Left $ InvalidRef l

        -- | Dereference
        Deref t'                          -> do (t'', store') <- evaluate' (t', store)
                                                Right (Deref t'', store')
                                            
        -- | Assignment
        Assign (Loc l) v
            | isVal v                     -> updateStore store l v                       
        Assign v1 t2 
            | isVal v1                    -> do (t2', store') <- evaluate' (t2, store)
                                                Right (Assign v1 t2', store')
        Assign t1 t2                      -> do (t1', store') <- evaluate' (t1, store)
                                                Right (Assign t1' t2, store')

        -- | Application
        App (Lambda _ t1 _) v2 
            | isVal v2                    -> Right (subsFromTop v2 t1, store)            
        App v1 t2 
            | isVal v1                    -> do (t2', store') <- evaluate' (t2, store)
                                                Right (App v1 t2', store')              
        App t1 t2                         -> do (t1', store') <- evaluate' (t1, store)
                                                Right (App t1' t2, store')              

        -- | Records
        Proj (Rec ls) l 
            | isVal $ Rec ls              -> do t' <- Rec ls `getVal` l                 
                                                Right (t', store)
        Proj (Rec ls) l                   -> do (t', store') <- evaluate' (Rec ls, store)
                                                Right (Proj t' l, store')            
        Rec ls 
            | not $ isVal $ Rec ls        -> evalRecord (t, store)                      
                        
        -- | No rules applied
        _                                 -> Left Stuck                                  

    -- | Big-step evaluation
    -- (apply evaluate' repeatedly until a value is reached or we're left with 
    -- an expression that cannot be evaluated further).
    evaluateToValue :: (Term, StoreEnv) -> Either RuntimeError (Term, StoreEnv)
    evaluateToValue x = case evaluate' x of
        Right res              -> evaluateToValue res
        Left Stuck             -> Right x      
        Left (Blame s1 s2 l _) -> Left $ Blame s1 s2 l $ fst x    
        Left err               -> Left err
    
    -- | Evaluate a term.
    evaluate :: Term -> Either RuntimeError Term
    evaluate t = case evaluateToValue (t, emptyStore) of
        Right (res, _) 
            | isUncoercedVal res -> Right res
            | otherwise          -> Left Stuck  -- Term is "stuck". 
        Left (Blame s1 s2 l t')  -> let cause = fromJust $ blame l t
                                        bres  = BlameRes cause t'
                                    in Left $ CastError s1 s2 bres  
        Left err                 -> Left err