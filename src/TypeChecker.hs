module TypeChecker where

    import Syntax
    import Types

    import Data.Maybe

    -- find the type for a term given a type environment
    typeOf :: Term -> Maybe Type
    typeOf t = case t of 
        Tru             -> Just Bool                                    -- (T-TRUE)
        Fls             -> Just Bool                                    -- (T-FALSE)
        Zero            -> Just Nat                                     -- (T-ZERO)

        Succ t'         -> do                                           -- (T-SUCC)
                            ty <- typeOf t' 
                            if ty == Nat 
                            then return Nat
                            else Nothing

        Pred t'         -> do                                           -- (T-PRED)
                            ty <- typeOf t' 
                            if ty == Nat 
                            then return Nat
                            else Nothing

        IsZero t'       -> do                                           -- (T-ISZERO)
                            ty <- typeOf t' 
                            if ty == Nat 
                            then return Bool
                            else Nothing

        If t1 t2 t3     -> do                                           -- (T-IF)
                            cond <- typeOf t1 
                            fst  <- typeOf t2 
                            snd  <- typeOf t3 
                            if cond == Bool && fst == snd 
                            then return fst
                            else Nothing
                            
        Var _ ty id     -> Just ty                                      -- (T-VAR)
        
        Lambda ty t' _  -> Arr ty <$> typeOf t'                         -- (T-ABS)

        App t1 t2       -> do                                           -- (T-APP)
                            Arr argTy retTy <- typeOf t1 
                            paramTy <- typeOf t2 
                            if argTy == paramTy 
                            then return retTy
                            else Nothing 

        _               -> Nothing                                      -- "Ill-typed"
         
    
        