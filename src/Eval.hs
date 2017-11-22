module Eval where
    -- define shifting and substitution here
    import Syntax
    
    import Data.Maybe
    import Data.Functor
    import Data.Typeable
    
    -- determine if a term is a value
    isVal :: Term -> Bool
    isVal t = case t of 
      Tru           -> True
      Fls           -> True
      Lambda _ _    -> True
      _             -> False
    
    shift :: Int -> Int -> Term -> Term
    shift c d t = case t of 
      Tru              -> t
      Fls              -> t
      Var k id         -> if k < c then Var k id else Var (k + d) id
      Lambda t1 ctx    -> Lambda (shift (c + 1) d t1) ctx 
      App t1 t2        -> App (shift c d t1) (shift c d t2)
      
    -- perform substitution given a variable with bruijn index j, a body s, and a term t
    subs :: Int -> Term -> Term -> Term 
    subs j s t = case t of 
      Var k id         -> if k == j then s else Var k id
      Lambda t1 ctx    -> Lambda (subs (j + 1) (shift 0 1 s) t1) ctx
      App t1 t2        -> App (subs j s t1) (subs j s t2)
    
    -- perform substitution from the beginning
    subsFromTop :: Term -> Term -> Term
    subsFromTop s t = shift 0 (-1) (subs 0 (shift 0 1 s) t)

    -- small-step evaluation
    eval' :: Term -> Maybe Term
    eval' t = case t of
      App (Lambda t1 _) v2 | isVal v2     -> Just (subsFromTop v2 t1) -- (E-APPABS)
      App t1 t2                           -> (\t1' -> App t1' t2) <$> eval' t1 -- (E-APP1)
      App v1 t2 | isVal v1                -> (\t2' -> App v1 t2') <$> eval' t2 -- (E-APP2)
      _                                   -> Nothing

    -- big-step evaluation
    -- (apply eval' repeatedly until a value is reached or we're left with an expression
    -- that cannot be evaluated further)
    nf :: Term -> Term
    nf x = fromMaybe x (nf <$> eval' x)
    
    -- evaluate a term
    eval :: Term -> Maybe Term
    eval t = case nf t of
      nft | isVal nft -> Just nft
          | otherwise -> Nothing -- term is "stuck"