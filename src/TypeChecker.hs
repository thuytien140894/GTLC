module TypeChecker (
    typeOf
  , isConsistent
  ) where

    import Syntax
    import Types
    import Errors
    import Subtype
    import Coercion

    -- find the type for a record
    rcdTypeOf :: Term -> Either TypeError Type
    rcdTypeOf (Rec [])                = Right $ TRec []
    rcdTypeOf (Rec ((l1, t1) : ys))   = case typeOf t1 of 
      Right ty   -> (`addType` (l1, ty)) <$> rcdTypeOf (Rec ys) 
      Left err   -> Left err

    -- add new entry to the record
    addType :: Type -> (String, Type) -> Type
    addType (TRec ls) newType = TRec (newType : ls)

    -- get the type for the specified field
    getType :: Type -> String -> Either TypeError Type
    getType (TRec []) l            = Left $ NotFound l 
    getType (TRec ((l1, ty) : ys)) l 
      | l1 == l                    = Right ty
      | otherwise                  = getType (TRec ys) l

    -- consistency rules
    isConsistent :: Type -> Type -> Bool
    isConsistent Dyn _                               = True
    isConsistent _ Dyn                               = True
    isConsistent ty1 ty2 | ty1 == ty2                = True
    isConsistent (Arr param1 ret1) (Arr param2 ret2) 
      | isConsistent param2 param1 &&
        isConsistent ret1 ret2                       = True
      | otherwise                                    = False
    isConsistent _ _                                 = False

    -- two types are compatible if either they are subtypes of one another or 
    -- consistent
    isCompatible :: Type -> Type -> Bool
    isCompatible ty1 ty2 = isSubtype ty1 ty2 || isConsistent ty1 ty2
    
    -- find the type for a term 
    typeOf :: Term -> Either TypeError Type
    typeOf t = case t of 
      Unit               -> Right TUnit                                   -- (T-UNIT)
      Tru                -> Right Bool                                    -- (T-TRUE)
      Fls                -> Right Bool                                    -- (T-FALSE)
      Zero               -> Right Nat                                     -- (T-ZERO)

      Succ t'            -> do                                            -- (T-SUCC)
                              ty <- typeOf t' 
                              case ty of  
                                Dyn -> Right Nat
                                Nat -> Right Nat
                                _   -> Left $ NotNat ty

      Pred t'            -> do                                            -- (T-PRED)
                              ty <- typeOf t' 
                              case ty of
                                Dyn -> Right Nat
                                Nat -> Right Nat
                                _   -> Left $ NotNat ty

      IsZero t'          -> do                                            -- (T-ISZERO)
                              ty <- typeOf t' 
                              case ty of 
                                Dyn  -> Right Bool
                                Nat  -> Right Bool
                                _    -> Left $ NotNat ty

      If t1 t2 t3        -> do                                            -- (T-IF)
                              cond <- typeOf t1 
                              fst  <- typeOf t2 
                              snd  <- typeOf t3 
                              case cond of 
                                Bool | fst == snd -> Right fst 
                                     | otherwise  -> Left $ Difference fst snd
                                _    -> Left $ NotBool cond
                          
      Rec ls             -> rcdTypeOf t                                   -- (T-RCD)

      Proj t l           -> case t of                                     -- (T-PROJ)
                              Rec ls -> case typeOf (Rec ls) of                       
                                          Right ty -> getType ty l
                                          Left err -> Left err
                              _      -> Left $ NotRecord t
                          
      Var _ ty _         -> case ty of                                    -- (T-VAR)
                              TUnit -> Left $ NotBound t   
                              _     -> Right ty                                  

      Lambda ty t' _     -> Arr ty <$> typeOf t'                          -- (T-ABS)     
      
      Cast c t           -> do                                            -- (T-CAST)
                              let (ty1, ty2) = getCoercionTypes c
                              ty <- typeOf t
                              case ty of 
                                s | s == ty1   -> Right ty2
                                  | otherwise  -> Left $ IllegalCast ty ty1

      App t1 t2          -> do                                            -- (T-APP1, T-APP2) + (T-SUB)
                              funcTy  <- typeOf t1   
                              argTy   <- typeOf t2                         
                              case funcTy of 
                                Dyn                              -> Right Dyn
                                Arr paramTy retTy 
                                  | argTy `isCompatible` paramTy -> Right retTy
                                  | otherwise                    -> Left $ Mismatch argTy paramTy
                                _                                -> Left $ NotFunction t1
         
    
        