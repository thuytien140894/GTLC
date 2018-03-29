module CastInserter where

  import TypeChecker
  import Syntax
  import Types
  import Coercion
  import Errors

  import Data.Maybe

  -- coerce argument type and the operator's parameter type
  getCoercion :: Term -> Term -> Either TypeError Coercion
  getCoercion t1 t2 = do 
    funcTy  <- typeOf t1
    argTy   <- typeOf t2
    case funcTy of 
      Arr paramTy _ -> return $ coerce argTy paramTy       -- (C-APP1)
      Dyn           -> return $ coerce argTy Dyn           -- (C-APP2)

  -- coerce a term to Nat
  getNatCoercion :: Term -> Either TypeError Coercion
  getNatCoercion t = (`coerce` Nat) <$> typeOf t

  -- coerce a term to Bool
  getBoolCoercion :: Term -> Either TypeError Coercion
  getBoolCoercion t = (`coerce` Bool) <$> typeOf t

  -- insert type casts
  insertCast :: Term -> Either TypeError Term
  insertCast e = case e of 
    App e1 e2        -> do 
                          t1 <- insertCast e1
                          t2 <- insertCast e2
                          c  <- getCoercion t1 t2
                          return $ App t1 (Cast c t2)
    Lambda ty e' ctx -> (\t' -> Lambda ty t' ctx) <$> insertCast e'
    Succ e'          -> do 
                          t' <- insertCast e'
                          c  <- getNatCoercion t'
                          return $ Succ (Cast c t')
    Pred e'          -> do 
                          t' <- insertCast e'
                          c  <- getNatCoercion t'
                          return $ Pred (Cast c t')
    IsZero e'        -> do 
                          t' <- insertCast e'
                          c  <- getBoolCoercion t'
                          return $ Succ (Cast c t')  
    _                -> Right e         