module TypeChecker (
  typeOf,
  typeCheck
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

  -- find the type for a record
  typeCheckRcd :: Term -> Label -> Either TypeError (Term, Type, Label)
  typeCheckRcd (Rec []) l               = Right (Rec [], TRec [], l)
  typeCheckRcd (Rec ((f1, t1) : ys)) l  = do 
    (t1', ty, l1) <- typeCheck' t1 l
    (rcd, rcdTy, l2) <- typeCheckRcd (Rec ys) l1 
    return (rcd `addField` (f1, t1'), rcdTy `addType` (f1, ty), l2)

  -- add new entry to the record type
  addType :: Type -> (String, Type) -> Type
  addType (TRec ls) newType = TRec (newType : ls)

  -- add new entry to the record
  addField :: Term -> (String, Term) -> Term
  addField (Rec ls) newField = Rec (newField : ls)

  -- get the type for the specified field
  getType :: Type -> String -> Either TypeError Type
  getType (TRec []) l            = Left $ NotFound l 
  getType (TRec ((l1, ty) : ys)) l 
    | l1 == l                    = Right ty
    | otherwise                  = TRec ys `getType` l

  -- typecheck a record field
  typeCheckField :: (Term, Type, Label) -> String -> Either TypeError (Term, Type, Label)
  typeCheckField (_, TRec [], _) f                           = Left $ NotFound f 
  typeCheckField (Rec ((_, t1) : xs), TRec ((f1, ty1) : ys), l) f 
    | f1 == f                                                = Right (t1, ty1, l)
    | otherwise                                              = typeCheckField (Rec xs, TRec ys, l) f

  -- typecheck a conditional
  typeCheckCond :: Term -> Label -> Either TypeError (Term, Type, Label)
  typeCheckCond (If e1 e2 e3) l = do
    (t1, cond, l1) <- typeCheck' e1 l
    (t2, fst, l2)  <- typeCheck' e2 l1
    (t3, snd, l3)  <- typeCheck' e3 l2
    case cond of 
      Dyn 
        | fst == snd             -> Right (If t1' t2 t3, fst, l4)  
        | fst `isConsistent` snd -> let (c1, l4)   = coerce fst snd l3
                                        (c2, l5)   = coerce snd fst l4
                                        (t2', t3') = (Cast c1 t2, Cast c2 t3)
                                    in Right (If t1' t2' t3', Dyn, l5) 
        | otherwise              -> Left $ Difference fst snd
        where t1'      = Cast c1 t1 
              (c1, l4) = coerce cond Bool l3
      Bool 
        | fst == snd             -> Right (If t1 t2 t3, fst, l3) 
        | fst `isConsistent` snd -> let (c2, l4)   = coerce fst snd l3
                                        (c3, l5)   = coerce snd fst l4
                                        (t2', t3') = (Cast c2 t2, Cast c3 t3)
                                    in Right (If t1 t2' t3', Dyn, l5) 
        | otherwise              -> Left $ Difference fst snd
      _                          -> Left $ NotBool cond

  -- typecheck an assignment 
  typeCheckAssignment :: Term -> Term -> Label -> Either TypeError (Term, Type, Label)
  typeCheckAssignment e1 e2 l = do
    (t1, ty1, l1) <- typeCheck' e1 l
    (t2, ty2, l2) <- typeCheck' e2 l1
    case ty1 of 
      TRef s      -> let (c, l3) = coerce ty2 s l2
                     in Right (t1 `Assign` Cast c t2, s, l3)
      Dyn         -> let (c1, l3) = (RefProj l2, increment l2)
                         (c2, l4) = coerce ty2 Dyn l3
                     in Right (Cast c1 t1 `Assign` Cast c2 t2, Dyn, l4)
      _           -> Left $ NotRef e1

  -- typecheck an application
  typeCheckApp :: Term -> Term -> Label -> Either TypeError (Term, Type, Label)
  typeCheckApp e1 e2 l = do                                            
    (t1, funcTy, l1)  <- typeCheck' e1 l  
    (t2, argTy, l2)   <- typeCheck' e2 l1
    case funcTy of 
      Dyn                              -> let (c1, l3) = (FuncProj l2, increment l2)  
                                              (c2, l4) = coerce argTy Dyn l3  
                                          in Right (Cast c1 t1 `App` Cast c2 t2, Dyn, l4)
      Arr paramTy retTy 
        | argTy `isSubtype` paramTy    -> Right (App t1 t2, retTy, l2)
        | argTy `isConsistent` paramTy -> let (c, l3) = coerce argTy paramTy l2 
                                          in Right (App t1 $ Cast c t2, retTy, l3)
        | otherwise                    -> Left $ Mismatch argTy paramTy
        where (c, l3) = coerce argTy paramTy l2
      _                                -> Left $ NotFunction e1

  -- two types are compatible if either they are subtypes of one another or 
  -- consistent
  isCompatible :: Type -> Type -> Bool
  isCompatible ty1 ty2 = isSubtype ty1 ty2 || isConsistent ty1 ty2
  
  -- TODO: Add typeOf() for reference, dereference, and assignment
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
                            Rec _  -> case typeOf t of                       
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

  -- typecheck the source term and insert cast if needed
  typeCheck' :: Term -> Label -> Either TypeError (Term, Type, Label)
  typeCheck' e l = case e of 
    Unit               -> Right (Unit, TUnit, l)                                -- (C-CONST)
    Tru                -> Right (e, Bool, l)                                    
    Fls                -> Right (e, Bool, l)                                   
    Zero               -> Right (e, Nat, l)                                     

    Succ e'            -> do                                                    -- (C-SUCC)
                            (t', ty, l1) <- typeCheck' e' l
                            case ty of  
                              Dyn -> let (c, l2) = coerce ty Nat l1
                                     in Right (Succ $ Cast c t', Nat, l2)
                              Nat -> Right (Succ t', Nat, l1)
                              _   -> Left $ NotNat ty

    Pred e'            -> do                                                    -- (C-PRED)
                            (t', ty, l1) <- typeCheck' e' l
                            case ty of
                              Dyn -> let (c, l2) = coerce ty Nat l1
                                     in Right (Pred $ Cast c t', Nat, l2)
                              Nat -> Right (Pred t', Nat, l1)
                              _   -> Left $ NotNat ty

    IsZero e'          -> do                                                    -- (C-ISZERO)
                            (t', ty, l1) <- typeCheck' e' l 
                            case ty of 
                              Dyn  -> let (c, l2) = coerce ty Nat l1
                                      in Right (IsZero $ Cast c t', Bool, l2)
                              Nat  -> Right (IsZero t', Bool, l1)
                              _    -> Left $ NotNat ty

    If e1 e2 e3        -> typeCheckCond e l                                     -- (C-IF)
                        
    Rec ls             -> typeCheckRcd e l                                      -- (C-RCD)

    Proj e' f          -> case e' of                                            -- (C-PROJ)
                            Rec _  -> do 
                                        res <- typeCheck' e' l 
                                        typeCheckField res f 
                            _      -> Left $ NotRecord e'
                        
    Var _ ty _         -> case ty of                                            -- (C-VAR)
                            TUnit -> Left $ NotBound e   
                            _     -> Right (e, ty, l)                           
                            
    Ref e'             -> do                                                    -- (C-REF) 
                            (t', ty, l1) <- typeCheck' e' l   
                            return (Ref t', TRef ty, l1)                             

    Deref e'           -> do                                                    -- (C-DEREF1 + C-DEREF2)
                            (t', ty, l1) <- typeCheck' e' l
                            case ty of 
                              Dyn     -> let (c, l2) = (RefProj l1, increment l1)
                                         in Right (Deref $ Cast c t', Dyn, l2)
                              TRef s  -> Right (Deref t', s, l)
                              _       -> Left $ NotRef e' 
                              
    Assign e1 e2       -> typeCheckAssignment e1 e2 l                           -- (C-ASSIGN1 + C-ASSIGN2)

    Lambda ty e' ctx   -> do                                                    -- (C-ABS)
                            (t', retTy, l') <- typeCheck' e' l           
                            Right (Lambda ty t' ctx, Arr ty retTy, l')  

    App e1 e2          -> typeCheckApp e1 e2 l                                  -- (C-APP1 + C-APP2)
        
  -- insert casts into a term
  typeCheck :: Term -> Either TypeError Term 
  typeCheck e = case typeCheck' e (Label 0) of 
    Right (t, _, _)      -> Right t 
    Left err             -> Left err