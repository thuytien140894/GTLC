module TypeChecker where

    import Syntax
    import Types
    import Prettier

    import Data.Maybe

    -- find the type for a term 
    typeOf :: Term -> Either String Type
    typeOf t = case t of 
        Tru             -> Right Bool                                    -- (T-TRUE)
        Fls             -> Right Bool                                    -- (T-FALSE)
        Zero            -> Right Nat                                     -- (T-ZERO)

        Succ t'         -> do                                            -- (T-SUCC)
                            ty <- typeOf t' 
                            case ty of  
                              Nat -> Right Nat
                              _   -> Left (printPretty t' ++ " is not of type Nat.")

        Pred t'         -> do                                            -- (T-PRED)
                            ty <- typeOf t' 
                            case ty of
                              Nat -> Right Nat
                              _   -> Left (printPretty t' ++ " is not of type Nat.")

        IsZero t'       -> do                                            -- (T-ISZERO)
                            ty <- typeOf t' 
                            case ty of 
                              Nat  -> Right Bool
                              _    -> Left (printPretty t' ++ " is not of type Nat.")

        If t1 t2 t3     -> do                                            -- (T-IF)
                            cond <- typeOf t1 
                            fst  <- typeOf t2 
                            snd  <- typeOf t3 
                            case cond of 
                              Bool | fst == snd -> Right fst 
                                   | otherwise  -> Left (printPretty t2 ++ " and " ++ printPretty t3 ++ " do not have the same type.") 
                              _    -> Left (printPretty t1 ++ " is not of type Bool.")
                            
        Var _ ty id     -> case ty of                                    -- (T-VAR)
                             TUnit -> Left "id has no bound type."   
                             _     -> Right ty                                  

        Lambda ty t' _  -> Arr ty <$> typeOf t'                          -- (T-ABS)

        App t1 t2       -> do 
                            funcTy  <- typeOf t1   
                            paramTy <- typeOf t2                                         -- (T-APP)
                            case funcTy of 
                              Arr argTy retTy | argTy == paramTy -> Right retTy
                                              | otherwise        -> Left (show t2 ++ " does not have the type of " ++ show argTy)

        _               -> Left "Ill-typed"                              -- "Ill-typed"
         
    
        