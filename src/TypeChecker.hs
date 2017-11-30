module TypeChecker where

    import Syntax
    import Types

    import Data.Maybe

    -- get type for variables
    getVarType :: String -> [(String, Type)] -> Maybe Type
    getVarType varName env = case env of 
        []              -> Nothing
        (id, ty) : ys  -> if varName == id then Just ty else getVarType varName ys

    -- find the type for a term given a type environment
    typeOf :: Term -> [(String, Type)] -> Maybe Type
    typeOf t env = case t of 
        Tru             -> Just Bool                                    -- (T-TRUE)
        Fls             -> Just Bool                                    -- (T-FALSE)
        Zero            -> Just Nat                                     -- (T-ZERO)

        Succ t'         -> do                                           -- (T-SUCC)
                            ty <- typeOf t' env
                            if ty == Nat 
                            then return Nat
                            else Nothing

        Pred t'         -> do                                           -- (T-PRED)
                            ty <- typeOf t' env
                            if ty == Nat 
                            then return Nat
                            else Nothing

        IsZero t'       -> do                                           -- (T-ISZERO)
                            ty <- typeOf t' env
                            if ty == Nat 
                            then return Bool
                            else Nothing

        If t1 t2 t3     -> do                                           -- (T-IF)
                            cond <- typeOf t1 env
                            fst  <- typeOf t2 env
                            snd  <- typeOf t3 env
                            if cond == Bool && fst == snd 
                            then return fst
                            else Nothing
                            
        Var _ id        -> getVarType id env                            -- (T-VAR)

        -- update the environment when encountering a function
        Lambda ty t' e  -> Arr ty <$> typeOf t' e                       -- (T-ABS)

        App t1 t2       -> do                                           -- (T-APP)
                            Arr argTy retTy <- typeOf t1 env
                            paramTy <- typeOf t2 env
                            if argTy == paramTy 
                            then return retTy
                            else Nothing 

        _               -> Nothing                                      -- "Ill-typed"
         
    
        