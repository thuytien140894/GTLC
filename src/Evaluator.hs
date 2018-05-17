module Evaluator 
    ( evaluate
    ) where

    import Coercion 
    import Error
    import GlobalState (SEvalState, BEvalState)
    import StoreEnv (StoreEnv)
    import Syntax
    import Utils

    import qualified GlobalState as GlobalS
    import qualified StoreEnv 
    
    import Control.Monad.Except (throwError)
    import Control.Monad.State (get, put)
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

    -- | Remove an enclosing coercion from a value 
    -- if the run-time type matches the target type.
    unbox :: Term -> SEvalState Term
    unbox (Cast c v) = do 
        env <- get
        case StoreEnv.typeOf v env of 
            srcTy 
                | srcTy `isConsistent` cstTy  -> return v
                | otherwise                   -> let bres = BlameRes None v
                                                 in throwError $ CastError srcTy cstTy bres
              where 
                cstTy = snd $ getCoercionTypes c

    -- | Small-step evaluation.
    evaluate' :: Term -> SEvalState Term
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
        Cast (Fail s1 s2 l) u             -> throwError $ Blame s1 s2 l                 
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
            | isVal v                     -> GlobalS.allocate v                             
        Ref t'                            -> Ref <$> evaluate' t'
        Deref (Loc l)                     -> GlobalS.peek l

        -- | Dereference
        Deref t'                          -> Deref <$> evaluate' t'
                                            
        -- | Assignment
        Assign (Loc l) v
            | isVal v                     -> GlobalS.update l v                       
        Assign v1 t2 
            | isVal v1                    -> Assign v1 <$> evaluate' t2
        Assign t1 t2                      -> (`Assign` t2) <$> evaluate' t1

        -- | Application
        App (Lambda _ t1 _) v2 
            | isVal v2                    -> return $ subsFromTop v2 t1          
        App v1 t2 
            | isVal v1                    -> App v1 <$> evaluate' t2
        App t1 t2                         -> (`App` t2) <$> evaluate' t1                        
                        
        -- | No rules applied
        _                                 -> throwError Stuck                                  

    -- | Big-step evaluation
    -- (apply evaluate' repeatedly until a value is reached or we're left with 
    -- an expression that cannot be evaluated further).
    evaluateToValue :: Term -> StoreEnv -> BEvalState Term
    evaluateToValue t env = do 
        put t
        let (res, newEnv) = GlobalS.runSEval (evaluate' t) env
        case res of 
            Right t'   -> evaluateToValue t' newEnv
            Left Stuck -> return t     
            Left err   -> throwError err
    
    -- | Evaluate a term.
    evaluate :: Term -> Either RuntimeError Term
    evaluate t = 
        let (res, t') = GlobalS.runBEval $ evaluateToValue t StoreEnv.empty
        in case res of
               Right t'' 
                   | isUncoercedVal t'' -> Right t''
                   | otherwise          -> Left Stuck 
               Left (Blame s1 s2 l)     -> let cause = fromJust $ blame l t
                                               bres  = BlameRes cause t'
                                           in Left $ CastError s1 s2 bres  
               Left err                 -> Left err 