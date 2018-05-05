module TypeChecker 
    ( typeCheck
    ) where

    import Coercion (coerce, isConsistent)
    import Error
    import GlobalState (TCheckState)
    import qualified GlobalState as GlobalS (runTyCheck, newLabel)
    import Subtype
    import Syntax
    import Type

    import Control.Monad.Except (throwError)

    -- -- | Find the type for a record.
    -- typeCheckRcd :: Term -> Label -> Either TypeError (Term, Type, Label)
    -- typeCheckRcd (Rec []) l              = Right (Rec [], TRec [], l)
    -- typeCheckRcd (Rec ((f1, t1) : ys)) l = do 
    --     (t1', ty, l1) <- typeCheck' t1 l
    --     (rcd, rcdTy, l2) <- typeCheckRcd (Rec ys) l1 
    --     return (rcd `addField` (f1, t1'), rcdTy `addType` (f1, ty), l2)

    -- -- | Typecheck a record field.
    -- typeCheckField :: (Term, Type, Label) -> String -> Either TypeError (Term, Type, Label)
    -- typeCheckField (_, TRec [], _) f = Left $ InvalidLabel f 
    -- typeCheckField (Rec ((_, t1) : xs), TRec ((f1, s1) : ys), l) f 
    --     | f1 == f                    = Right (t1, s1, l)
    --     | otherwise                  = typeCheckField (Rec xs, TRec ys, l) f

    -- | Typecheck a conditional.
    typeCheckCond :: Term -> TCheckState (Term, Type)
    typeCheckCond (If e1 e2 e3) = do
        (t1, cond) <- typeCheck' e1 
        (t2, fst)  <- typeCheck' e2 
        (t3, snd)  <- typeCheck' e3
        case cond of 
            Dyn 
                -- | Two branches have the same type, so 
                -- there is no need for casting.
                | fst == snd             -> do c1 <- coerce cond Bool
                                               let t1' = Cast c1 t1
                                               return (If t1' t2 t3, fst)  
                | fst `isConsistent` snd -> do c1 <- coerce cond Bool
                                               c2 <- coerce fst snd
                                               c3 <- coerce snd fst 
                                               let (t1', t2', t3') = (Cast c1 t1, Cast c2 t2, Cast c3 t3)
                                               return (If t1' t2' t3', Dyn)   
                -- | Two branches have different types.
                | otherwise              -> throwError $ Difference fst snd
            Bool 
                | fst == snd             -> return (If t1 t2 t3, fst) 
                | fst `isConsistent` snd -> do c2 <- coerce fst snd 
                                               c3 <- coerce snd fst
                                               let (t2', t3') = (Cast c2 t2, Cast c3 t3)
                                               return (If t1 t2' t3', Dyn) 
                | otherwise              -> throwError $ Difference fst snd
            _                            -> throwError $ NotBool cond

    -- | Typecheck an assignment. 
    typeCheckAssignment :: Term -> TCheckState (Term, Type)
    typeCheckAssignment (Assign e1 e2) = do
        (t1, s1) <- typeCheck' e1 
        (t2, s2) <- typeCheck' e2 
        case s1 of 
            TRef s 
                | s2 == s             -> return (t1 `Assign` t2, s)
                | s2 `isConsistent` s -> do c <- coerce s2 s
                                            return (t1 `Assign` Cast c t2, s)
                | otherwise           -> throwError $ AssignMismatch s2 s (Assign e1 e2)
            Dyn                       -> do l  <- GlobalS.newLabel
                                            c2 <- coerce s2 Dyn 
                                            return (Cast (RefProj l) t1 `Assign` Cast c2 t2, Dyn)
            _                         -> throwError $ IllegalAssign e1

    -- | Typecheck an application.
    typeCheckApp :: Term -> TCheckState (Term, Type)
    typeCheckApp (App e1 e2) = do                                            
        (t1, funcTy) <- typeCheck' e1   
        (t2, argTy)  <- typeCheck' e2
        case funcTy of 
            Dyn                                -> do l  <- GlobalS.newLabel 
                                                     c2 <- coerce argTy Dyn   
                                                     return (Cast (FuncProj l) t1 `App` Cast c2 t2, Dyn)
            Arr paramTy retTy 
                | argTy `isSubtype` paramTy    -> return (App t1 t2, retTy)
                | argTy `isConsistent` paramTy -> do c <- coerce argTy paramTy  
                                                     return (App t1 $ Cast c t2, retTy)
                | otherwise                    -> throwError $ FunMismatch argTy paramTy (App e1 e2)
            _                                  -> throwError $ NotFunction e1                                                   

    -- | Typecheck the source term and insert cast if needed
    typeCheck' :: Term -> TCheckState (Term, Type)
    typeCheck' e = case e of 
        -- | Constants
        Unit              -> return (Unit, TUnit)                                
        Tru               -> return (e, Bool)                                    
        Fls               -> return (e, Bool)                                   
        Zero              -> return (e, Nat)                                     
        
        -- | Arithmetic
        Succ e'           -> do (t', ty) <- typeCheck' e'
                                case ty of  
                                    Dyn -> do c <- coerce ty Nat
                                              return (Succ $ Cast c t', Nat)
                                    Nat -> return (Succ t', Nat)
                                    _   -> throwError $ NotNat ty
        Pred e'           -> do (t', ty) <- typeCheck' e'
                                case ty of
                                    Dyn -> do c <- coerce ty Nat
                                              return (Pred $ Cast c t', Nat)
                                    Nat -> return (Pred t', Nat)
                                    _   -> throwError $ NotNat ty
        IsZero e'         -> do (t', ty) <- typeCheck' e' 
                                case ty of 
                                    Dyn -> do c <- coerce ty Nat
                                              return (IsZero $ Cast c t', Bool)
                                    Nat -> return (IsZero t', Bool)
                                    _   -> throwError $ NotNat ty

        -- | Conditional
        If e1 e2 e3       -> typeCheckCond e                                      
                 
        -- -- | Records
        -- Rec ls            -> typeCheckRcd e l
        -- Proj e' f         -> case e' of                                            
        --                          Rec _ -> do res <- typeCheck' e' l 
        --                                      typeCheckField res f 
        --                          _     -> Left $ NotRecord e'
           
        -- | Variables
        Var _ ty _        -> case ty of                                            
                                 TUnit -> throwError $ NotBound e   
                                 _     -> return (e, ty)                           
                    
        -- | Reference
        Ref e'            -> do (t', ty) <- typeCheck' e'   
                                return (Ref t', TRef ty)                             

        -- | Dereference
        Deref e'          -> do (t', ty) <- typeCheck' e' 
                                case ty of 
                                    Dyn    -> do l <- GlobalS.newLabel
                                                 return (Deref $ Cast (RefProj l) t', Dyn)
                                    TRef s -> return (Deref t', s)
                                    _      -> throwError $ IllegalDeref e' 
                              
        -- | Assignment
        Assign e1 e2     -> typeCheckAssignment e                               

        -- | Abstraction
        Lambda ty e' ctx -> do (t', retTy) <- typeCheck' e'            
                               return (Lambda ty t' ctx, Arr ty retTy)  

        -- -- | Application
        App e1 e2        -> typeCheckApp e                                    
            
    -- | Typecheck an initial label. Return an AST or an error.
    typeCheck :: Term -> Either TypeError Term 
    typeCheck e = case GlobalS.runTyCheck $ typeCheck' e of
        Right (t, _) -> Right t 
        Left err     -> Left err