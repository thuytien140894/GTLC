module TypeChecker (
  typeCheck
  ) where

  import Syntax
  import Types
  import Errors
  import Subtype
  import Coercion
  import Utils (addField, addType)

  -- two types are compatible if either they are subtypes of one another or 
  -- consistent
  isCompatible :: Type -> Type -> Bool
  isCompatible s t = isSubtype s t || isConsistent s t 

  -- find the type for a record
  typeCheckRcd :: Term -> Label -> Either TypeError (Term, Type, Label)
  typeCheckRcd (Rec []) l               = Right (Rec [], TRec [], l)
  typeCheckRcd (Rec ((f1, t1) : ys)) l  = do 
    (t1', ty, l1) <- typeCheck' t1 l
    (rcd, rcdTy, l2) <- typeCheckRcd (Rec ys) l1 
    return (rcd `addField` (f1, t1'), rcdTy `addType` (f1, ty), l2)

  -- typecheck a record field
  typeCheckField :: (Term, Type, Label) -> String -> Either TypeError (Term, Type, Label)
  typeCheckField (_, TRec [], _) f                           = Left $ InvalidLabel f 
  typeCheckField (Rec ((_, t1) : xs), TRec ((f1, s1) : ys), l) f 
    | f1 == f                                                = Right (t1, s1, l)
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
  typeCheckAssignment :: Term -> Label -> Either TypeError (Term, Type, Label)
  typeCheckAssignment (Assign e1 e2) l = do
    (t1, s1, l1) <- typeCheck' e1 l
    (t2, s2, l2) <- typeCheck' e2 l1
    case s1 of 
      TRef s 
        | s2 == s                 -> Right (t1 `Assign` t2, s, l2)
        | s2 `isConsistent` s     -> let (c, l3) = coerce s2 s l2
                                     in Right (t1 `Assign` Cast c t2, s, l3)
        | otherwise               -> Left $ AssignMismatch s2 s
      Dyn                         -> let (c1, l3) = (RefProj l2, increment l2)
                                         (c2, l4) = coerce s2 Dyn l3
                                     in Right (Cast c1 t1 `Assign` Cast c2 t2, Dyn, l4)
      _                           -> Left $ IllegalAssign e1

  -- typecheck an application
  typeCheckApp :: Term -> Label -> Either TypeError (Term, Type, Label)
  typeCheckApp (App e1 e2) l = do                                            
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
        | otherwise                    -> Left $ FunMismatch argTy paramTy
      _                                -> Left $ NotFunction e1                                                   

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
                              TRef s  -> Right (Deref t', s, l1)
                              _       -> Left $ IllegalDeref e' 
                              
    Assign e1 e2       -> typeCheckAssignment e l                               -- (C-ASSIGN1 + C-ASSIGN2)

    Lambda ty e' ctx   -> do                                                    -- (C-ABS)
                            (t', retTy, l') <- typeCheck' e' l           
                            Right (Lambda ty t' ctx, Arr ty retTy, l')  

    App e1 e2          -> typeCheckApp e l                                      -- (C-APP1 + C-APP2)
        
  -- insert casts into a term
  typeCheck :: Term -> Either TypeError Term 
  typeCheck e = case typeCheck' e (Label 0) of 
    Right (t, _, _)      -> Right t 
    Left err             -> Left err