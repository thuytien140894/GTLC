module ParseUtils where 

    import Syntax
    import Type

    import Data.List (elemIndex)
    import Data.Maybe

    -- | Get the bruijn index for a free variable.
    getBruijnIndex :: String -> [String] -> [String] -> Int
    getBruijnIndex id freeVars boundVars = fromJust freeIndex + length boundVars
      where 
        freeIndex  = elemIndex id $ reverse freeVars

    -- | Correct the bruijn index for a bound variable.
    -- This function is called when parsing a Lambda term. 
    fixBinding :: Term -> String -> Int -> Term
    fixBinding t x b = case t of
        Var _ ty id 
            | id == x    -> Var b ty id
        Succ t'          -> Succ $ fixBinding t' x b
        Pred t'          -> Pred $ fixBinding t' x b
        IsZero t'        -> IsZero $ fixBinding t' x b
        If t1 t2 t3      -> let t1' = fixBinding t1 x b
                                t2' = fixBinding t2 x b
                                t3' = fixBinding t3 x b
                            in If t1' t2' t3'
        Ref t'           -> Ref $ fixBinding t' x b
        Deref t'         -> Deref $ fixBinding t' x b
        Assign t1 t2     -> let t1' = fixBinding t1 x b
                                t2' = fixBinding t2 x b
                            in Assign t1' t2'
        Lambda ty t' ctx -> Lambda ty (fixBinding t' x $ b + 1) ctx
        App t1 t2        -> fixBinding t1 x b `App` fixBinding t2 x b
        _                -> t

    -- | Fix Bruijn indices for free variables.
    fixFreeBinding :: Term -> [String] -> [String] -> Term
    fixFreeBinding t freeVars boundVars = case t of
        Var _ ty id 
            | id `elem` freeVars -> Var (getBruijnIndex id freeVars boundVars) ty id
        Succ t'                  -> Succ $ fixFreeBinding t' freeVars boundVars
        Pred t'                  -> Pred $ fixFreeBinding t' freeVars boundVars
        IsZero t'                -> IsZero $ fixFreeBinding t' freeVars boundVars
        If t1 t2 t3              -> let t1' = fixFreeBinding t1 freeVars boundVars
                                        t2' = fixFreeBinding t2 freeVars boundVars
                                        t3' = fixFreeBinding t3 freeVars boundVars
                                    in If t1' t2' t3'
        Ref t'                   -> Ref $ fixFreeBinding t' freeVars boundVars
        Deref t'                 -> Deref $ fixFreeBinding t' freeVars boundVars
        Assign t1 t2             -> let t1' = fixFreeBinding t1 freeVars boundVars
                                        t2' = fixFreeBinding t2 freeVars boundVars
                                    in Assign t1' t2'
        Lambda ty t' ctx         -> Lambda ty (fixFreeBinding t' freeVars boundVars) ctx
        App t1 t2                -> fixFreeBinding t1 freeVars boundVars 
                                    `App` fixFreeBinding t2 freeVars boundVars
        _                        -> t

    -- | Update the typing environment for nested lambdas when new 
    --bound variables are introduced.
    updateVarType :: Term -> String -> Type -> Term
    updateVarType t x newTy = case t of 
        Var k _ id 
            | id == x    -> Var k newTy id
        Succ t'          -> Succ $ updateVarType t' x newTy
        Pred t'          -> Pred $ updateVarType t' x newTy
        IsZero t'        -> IsZero $ updateVarType t' x newTy
        If t1 t2 t3      -> let t1' = updateVarType t1 x newTy
                                t2' = updateVarType t2 x newTy
                                t3' = updateVarType t3 x newTy
                            in If t1' t2' t3'
        Ref t'           -> Ref $ updateVarType t' x newTy
        Deref t'         -> Deref $ updateVarType t' x newTy
        Assign t1 t2     -> let t1' = updateVarType t1 x newTy
                                t2' = updateVarType t2 x newTy
                            in Assign t1' t2'
        Lambda ty t' ctx -> Lambda ty (updateVarType t' x newTy) ctx 
        App t1 t2        -> updateVarType t1 x newTy `App` updateVarType t2 x newTy
        _                -> t

    -- | Retrieve the binding context of an abstraction.
    getBoundVar :: Term -> [String]
    getBoundVar t = case t of
        Lambda _ _ ctx -> ctx
        Succ t'        -> getBoundVar t'
        Pred t'        -> getBoundVar t'
        IsZero t'      -> getBoundVar t'
        If t1 t2 t3    -> getBoundVar t1 ++ getBoundVar t2 ++ getBoundVar t3
        Ref t'         -> getBoundVar t'
        Deref t'       -> getBoundVar t'
        Assign t1 t2   -> getBoundVar t1 ++ getBoundVar t2
        App t1 t2      -> getBoundVar t1 ++ getBoundVar t2
        _              -> []

    -- | Find free variables.
    getFreeVar :: Term -> [String] -> [String]
    getFreeVar t boundVars = case t of
        Lambda _ t1 _ -> getFreeVar t1 boundVars
        Succ t'       -> getFreeVar t' boundVars
        Pred t'       -> getFreeVar t' boundVars
        IsZero t'     -> getFreeVar t' boundVars
        If t1 t2 t3   -> let l1 = getFreeVar t1 boundVars
                             l2 = getFreeVar t2 boundVars
                             l3 = getFreeVar t3 boundVars
                         in l1 ++ l2 ++ l3
        Ref t'        -> getFreeVar t' boundVars
        Deref t'      -> getFreeVar t' boundVars
        Assign t1 t2  -> let l1 = getFreeVar t1 boundVars
                             l2 = getFreeVar t2 boundVars
                         in l1 ++ l2
        App t1 t2     -> getFreeVar t1 boundVars ++ getFreeVar t2 boundVars
        Var _ _ id    -> if id `elem` boundVars 
                             then [] 
                             else [id]
        _             -> []  

    -- | Application.
    apply :: Term -> Term -> Term
    apply Unit t2 = t2
    apply t1 t2   = App t1 t2

    -- | Recursively apply terms from the left.
    applyFromLeft :: [Term] -> Term
    applyFromLeft = foldl apply Unit

    -- | "Arrow" two types.
    arrow :: Type -> Type -> Type
    arrow t2 TUnit = t2
    arrow t1 t2    = Arr t1 t2

    -- | Recursively "arrow" types from the right.
    arrowFromRight :: [Type] -> Type
    arrowFromRight = foldr arrow TUnit