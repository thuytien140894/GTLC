module TypeChecker (
  typeOf
  ) where

    import Syntax
    import Types
    import TypeErrors

    -- find the type for a record
    rcdTypeOf :: Term -> Either TypeError Type
    rcdTypeOf (Rec [])              = Right $ TRec []
    rcdTypeOf (Rec ((l1, t1) : ys))   = case typeOf t1 of 
      Right ty   -> (`addType` (l1, ty)) <$> rcdTypeOf (Rec ys) 
      Left err   -> Left err

    -- add new entry to the record
    addType :: Type -> (String, Type) -> Type
    addType (TRec ls) newType = TRec (newType : ls)

    -- get the type for the specified field
    getType :: Type -> String -> Either TypeError Type
    getType (TRec []) l             = Left $ NotFound l 
    getType (TRec ((l1, ty) : ys)) l 
       | l1 == l                    = Right ty
       | otherwise                  = getType (TRec ys) l

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
                            
        Rec ls          -> rcdTypeOf t                                   -- (T-RCD)

        Proj (Rec ls) l -> case typeOf (Rec ls) of                       -- (T-PROJ)
                             Right ty -> getType ty l
                             Left err -> Left err
                            
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
         
    
        