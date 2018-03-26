module CastInserter where

  import TypeChecker
  import Syntax
  import Types
  import Coercion
  import TypeErrors

  import Data.Maybe

  -- coerce argument type and the operator's parameter type
  getCoercion :: Term -> Term -> Either TypeError Coercion
  getCoercion t1 t2 = do 
    Arr paramTy _  <- typeOf t1
    argTy          <- typeOf t2
    return $ coerce argTy paramTy

  -- insert type casts
  insertCast :: Term -> Either TypeError Term
  insertCast (App e1 e2) = do 
    t1 <- insertCast e1
    t2 <- insertCast e2
    c  <- getCoercion t1 t2
    return $ App t1 (Cast c t2)
  insertCast t = Right t              