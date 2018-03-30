module CastInserter where

  import TypeChecker
  import Syntax
  import Types
  import Coercion
  import Errors

  import Data.Maybe

  -- coerce argument type and the operator's parameter type
  getCoercion :: Term -> Term -> LabelIndex -> Either TypeError (Coercion, LabelIndex)
  getCoercion t1 t2 l = do 
    funcTy  <- typeOf t1
    argTy   <- typeOf t2
    case funcTy of 
      Arr paramTy _ -> return $ coerce argTy paramTy l     -- (C-APP1)
      Dyn           -> return $ coerce argTy Dyn l         -- (C-APP2)

  -- coerce a term to Nat
  getNatCoercion :: Term -> LabelIndex -> Either TypeError (Coercion, LabelIndex)
  getNatCoercion t l = (\t' -> coerce t' Nat l) <$> typeOf t

  -- coerce a term to Bool
  getBoolCoercion :: Term -> LabelIndex -> Either TypeError (Coercion, LabelIndex)
  getBoolCoercion t l = (\t' -> coerce t' Bool l) <$> typeOf t

  -- insert type casts
  insertCast :: Term -> LabelIndex -> Either TypeError (Term, LabelIndex)
  insertCast e l = case e of 
    App e1 e2        -> do 
                          (t1, l1) <- insertCast e1 l
                          (t2, l2) <- insertCast e2 l1
                          (c, l3)  <- getCoercion t1 t2 l2
                          let t = App t1 (Cast c t2)
                          return (t, l3)
    Lambda ty e' ctx -> do 
                          (t', l') <- insertCast e' l
                          let t = Lambda ty t' ctx
                          return (t, l')
    Succ e'          -> do 
                          (t', l1) <- insertCast e' l
                          (c, l2)  <- getNatCoercion t' l1
                          let t = Succ (Cast c t')
                          return (t, l2)
    Pred e'          -> do 
                          (t', l1) <- insertCast e' l
                          (c, l2)  <- getNatCoercion t' l1
                          let t = Pred (Cast c t')
                          return (t, l2)
    IsZero e'        -> do 
                          (t', l1) <- insertCast e' l
                          (c, l2)  <- getBoolCoercion t' l1
                          let t = IsZero (Cast c t')
                          return (t, l2)
    _                -> return (e, l)        