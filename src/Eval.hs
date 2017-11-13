module Eval where
    -- define shifting and substitution here
    import Syntax
    
    import Data.Maybe
    import Data.Functor
    import Data.Typeable
    
    -- a term is a value if it is an abstraction
    isVal :: Term -> Bool
    isVal t = case t of 
      Tr           -> True
      Fl           -> True
      Lambda _     -> True
      _            -> False
    
    shift :: Int -> Int -> Term -> Term
    shift c d t = case t of 
      Tr            -> t
      Fl            -> t
      Var k         -> if k < c then Var k else Var (k + d)
      Lambda t1     -> Lambda (shift (c + 1) d t1)  
      App t1 t2     -> App (shift c d t1) (shift c d t2)
      
    subs :: Int -> Term -> Term -> Term 
    subs j s t = case t of 
      Var k         -> if k == j then s else Var k 
      Lambda t1     -> Lambda (subs (j + 1) (shift 0 1 s) t1)
      App t1 t2     -> App (subs j s t1) (subs j s t2)
    
    subsFromTop :: Term -> Term -> Term
    subsFromTop s t = shift 0 (-1) (subs 0 (shift 0 1 s) t)

    eval' :: Term -> Maybe Term
    eval' t = case t of
      App (Lambda t1) v2 | isVal v2     -> Just (subsFromTop v2 t1) -- (E-APPABS)
      App t1 t2                         -> (\t1' -> App t1' t2) <$> eval' t1 -- (E-APP1)
      App v1 t2 | isVal v1              -> (\t2' -> App v1 t2') <$> eval' t2 -- (E-APP2)
      _                                 -> Nothing

    -- apply eval' repeatedly until a value is reached or we're left with an expression
    -- that cannot be evaluated further
    -- for example, if we evaluate "If Tr c _", Just c is returned but c may be further 
    -- evaluated.
    -- recursive function
    -- multi-step evaluation
    nf :: Term -> Term
    nf x = fromMaybe x (nf <$> eval' x)
    
    eval :: Term -> Maybe Term
    eval t = case nf t of
      nft | isVal nft -> Just nft
          | otherwise -> Nothing -- term is "stuck"