module Utils where

    import Error 
    import Syntax

    import Control.Applicative ((<|>))

    -- | Renumber the indices of free variables in a term by
    -- maintaining the "cutoff" parameter c that controls which 
    -- variables should be shifted. i.e. Variables with indices less 
    -- than c are bound and therefore should stay the same.
    shift :: Int -> Int -> Term -> Term
    shift a b t = case t of 
        Var k ty id      -> if k < a 
                                then t 
                                else Var (k + b) ty id
        Cast c t'        -> Cast c $ shift a b t'
        Succ t'          -> Succ $ shift a b t'
        Pred t'          -> Pred $ shift a b t'
        IsZero t'        -> IsZero $ shift a b t'
        If t1 t2 t3      -> let t1' = shift a b t1
                                t2' = shift a b t2 
                                t3' = shift a b t3
                            in If t1' t2' t3'
        Lambda ty t' ctx -> Lambda ty (shift (a + 1) b t') ctx
        Ref t'           -> Ref $ shift a b t'
        Deref t'         -> Deref $ shift a b t'
        Assign t1 t2     -> shift a b t1 `Assign` shift a b t2
        App t1 t2        -> shift a b t1 `App` shift a b t2
        _                -> t  -- t is a constant.
        
    -- | Perform substitution given a variable with bruijn index j, 
    -- a substituting term s, and a body t.
    subs :: Int -> Term -> Term -> Term 
    subs j s t = case t of 
        Var k ty id      -> if k == j 
                                then s 
                                else t
        Cast c t'        -> Cast c $ subs j s t'
        Succ t'          -> Succ $ subs j s t'
        Pred t'          -> Pred $ subs j s t'
        IsZero t'        -> IsZero $ subs j s t'
        If t1 t2 t3      -> let t1' = subs j s t1
                                t2' = subs j s t2 
                                t3' = subs j s t3
                            in If t1' t2' t3'
        Lambda ty t' ctx -> Lambda ty (subs (j + 1) (shift 0 1 s) t') ctx
        Ref t'           -> Ref $ subs j s t'
        Deref t'         -> Deref $ subs j s t'
        Assign t1 t2     -> subs j s t1 `Assign` subs j s t2
        App t1 t2        -> subs j s t1 `App` subs j s t2
        _                -> t  -- t is a constant.

    -- | Check if a term is singular (constants and variables).
    isSingular :: Term -> Bool
    isSingular Var{} = True
    isSingular Zero  = True
    isSingular Tru   = True 
    isSingular Fls   = True
    isSingular _     = False 

    -- | Retrieve the term with a failed cast.
    getFailedTerm :: Term -> Maybe Term 
    getFailedTerm t = case t of 
        Cast Fail{} t' -> Just t'
        Succ t'        -> getFailedTerm t'
        Pred t'        -> getFailedTerm t'
        IsZero t'      -> getFailedTerm t'
        If t1 t2 t3    -> getFailedTerm t1 
                          <|> getFailedTerm t2 
                          <|> getFailedTerm t3
        Lambda _ t' _  -> getFailedTerm t'
        App t1 t2      -> getFailedTerm t1 <|> getFailedTerm t2
        Ref t'         -> getFailedTerm t'
        Deref t'       -> getFailedTerm t'
        Assign t1 t2   -> getFailedTerm t1 <|> getFailedTerm t2
        Cast c t'      -> getFailedTerm t'
        _              -> Nothing

    -- | Remove casts from an expression.
    removeCasts :: Term -> Term 
    removeCasts t = case t of 
        Cast c t'        -> removeCasts t'
        Succ t'          -> Succ $ removeCasts t'
        Pred t'          -> Pred $ removeCasts t'
        IsZero t'        -> IsZero $ removeCasts t'
        If t1 t2 t3      -> let t1' = removeCasts t1
                                t2' = removeCasts t2 
                                t3' = removeCasts t3
                            in If t1' t2' t3'
        Lambda ty t' ctx -> Lambda ty (removeCasts t') ctx
        Ref t'           -> Ref $ removeCasts t'
        Deref t'         -> Deref $ removeCasts t'
        Assign t1 t2     -> removeCasts t1 `Assign` removeCasts t2
        App t1 t2        -> removeCasts t1 `App` removeCasts t2
        _                -> t  -- t is a constant.

    -- | Check if a coercion is to blame given a blame label.
    blameCoercion :: Label -> Coercion -> Cause -> Maybe Cause
    blameCoercion l c cause = case c of 
        FuncProj l'
            | l' == l -> Just Function
        Fail _ _ l'   
            | l' == l -> Just cause
        RefProj l'               
            | l' == l -> Just Reference
        Project _ l' 
            | l' == l -> Just cause
        Func c1 c2    -> blameCoercion l c1 FunArg 
                         <|> blameCoercion l c2 FunRet
        CRef c1 c2    -> blameCoercion l c1 RefWrite 
                         <|> blameCoercion l c2 RefRead
        Seq c1 c2     -> blameCoercion l c1 cause 
                         <|> blameCoercion l c2 cause
        _             -> Nothing 

    -- | Find the coercion responsible for the blame. 
    blame :: Label -> Term -> Maybe Cause
    blame l t = case t of 
        Succ t'       -> blame l t'
        Pred t'       -> blame l t'
        IsZero t'     -> blame l t'
        If t1 t2 t3   -> blame l t1 <|> blame l t2 <|> blame l t3
        Lambda _ t' _ -> blame l t'
        App t1 t2     -> blame l t1 <|> blame l t2
        Ref t'        -> blame l t'
        Deref t'      -> blame l t'
        Assign t1 t2  -> blame l t1 <|> blame l t2
        Cast c t'     -> blameCoercion l c None <|> blame l t'
        _             -> Nothing