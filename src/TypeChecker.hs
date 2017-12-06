module TypeChecker where

    import Syntax
    import Types
    import TypeErrors
    import Prettier

    import Data.Maybe

    -- find the type for a term 
    typeOf :: Term -> Either TypeError Type
    typeOf t = case t of 
        Tru             -> Right Bool                                    -- (T-TRUE)
        Fls             -> Right Bool                                    -- (T-FALSE)
        Zero            -> Right Nat                                     -- (T-ZERO)

        Succ t'         -> do                                            -- (T-SUCC)
                            ty <- typeOf t' 
                            case ty of  
                              Nat -> Right Nat
                              _   -> Left $ Mismatch t' Nat

        Pred t'         -> do                                            -- (T-PRED)
                            ty <- typeOf t' 
                            case ty of
                              Nat -> Right Nat
                              _   -> Left $ Mismatch t' Nat

        IsZero t'       -> do                                            -- (T-ISZERO)
                            ty <- typeOf t' 
                            case ty of 
                              Nat  -> Right Bool
                              _    -> Left $ Mismatch t' Bool

        If t1 t2 t3     -> do                                            -- (T-IF)
                            cond <- typeOf t1 
                            fst  <- typeOf t2 
                            snd  <- typeOf t3 
                            case cond of 
                              Bool | fst == snd -> Right fst 
                                   | otherwise  -> Left $ Difference t2 t3
                              _    -> Left $ Mismatch t1 Bool
                            
        Var _ ty id     -> case ty of                                    -- (T-VAR)
                             TUnit -> Left $ NotBound t   
                             _     -> Right ty                                  

        Lambda ty t' _  -> Arr ty <$> typeOf t'                          -- (T-ABS)

        App t1 t2       -> do                                            -- (T-APP)
                            funcTy  <- typeOf t1   
                            paramTy <- typeOf t2                         
                            case funcTy of 
                              Arr argTy retTy | argTy == paramTy -> Right retTy
                                              | otherwise        -> Left $ Mismatch t2 argTy
                              _                                  -> Left $ NotFunction t1

        _               -> Left IllTyped                                 -- "Ill-typed"
         
    
        