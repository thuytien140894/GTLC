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

  -- insert type casts
  insertCast :: Term -> Either TypeError Term
  insertCast t = case t of 
    App e1 e2         -> do 
                          t1 <- insertCast e1
                          t2 <- insertCast e2
                          c <- getCoercion t1 t2
                          return $ App t1 (Cast c t2)
    Lambda ty t' ctx  -> (\t'' -> Lambda ty t'' ctx) <$> insertCast t'
    Succ t'           -> Succ <$> insertCast t'
    Pred t'           -> Pred <$> insertCast t'
    IsZero t'         -> IsZero <$> insertCast t'
    _                 -> Right t -- t is a constant            