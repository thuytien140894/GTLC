module Evaluator ( 
  evaluate
  ) where

    import Syntax
    import Coercion
    
    import Data.Maybe
    import Data.Functor
    
    -- determine if a list contains all values
    areAllVal :: [Entry] -> Bool
    areAllVal []            = True
    areAllVal ((l1, t1) : ys)  
      | isVal t1            = areAllVal ys
      | otherwise           = False

    -- determine if a term is an uncoerced value
    isUncoercedVal :: Term -> Bool
    isUncoercedVal t = case t of 
      Unit                  -> True
      Tru                   -> True
      Fls                   -> True
      t' | isNumeric t'     -> True
      Lambda {}             -> True
      Rec ls                -> areAllVal ls
      _                     -> False

    -- determine if a term is a coerced value
    isCoercedValue :: Term -> Bool
    isCoercedValue t = case t of 
      Cast _ v | isUncoercedVal v  -> True 
      _                            -> False

    -- determine if a term is a value
    isVal :: Term -> Bool
    isVal t = isUncoercedVal t || isCoercedValue t
    
    -- determine if a term is a numeric value
    isNumeric :: Term -> Bool
    isNumeric t = case t of 
      Zero                   -> True
      Succ t'                -> isNumeric t'
      _                      -> False

    -- renumber the indices of free variables in a term
    -- maintain the "cutoff" parameter c that controls which variables should be shifted
    -- i.e. variables with indices less than c are bound and therefore should stay the same
    shift :: Int -> Int -> Term -> Term
    shift c d t = case t of 
      Var k ty id      -> if k < c then t else Var (k + d) ty id
      Lambda ty t1 ctx -> Lambda ty (shift (c + 1) d t1) ctx 
      App t1 t2        -> App (shift c d t1) (shift c d t2)
      _                -> t -- t is a constant
      
    -- perform substitution given a variable with bruijn index j, a body s, and a term t
    subs :: Int -> Term -> Term -> Term 
    subs j s t = case t of 
      Var k ty id      -> if k == j then s else t
      Lambda ty t1 ctx -> Lambda ty (subs (j + 1) (shift 0 1 s) t1) ctx
      App t1 t2        -> App (subs j s t1) (subs j s t2)
      _                -> t -- t is a constant
    
    -- perform substitution from the beginning
    subsFromTop :: Term -> Term -> Term
    subsFromTop s t = shift 0 (-1) (subs 0 (shift 0 1 s) t)

    -- get the value for the specified field
    getVal :: Term -> String -> Maybe Term
    getVal (Rec []) _               = Nothing
    getVal (Rec ((l1, t1) : ys)) l 
      | l1 == l                     = Just t1
      | otherwise                   = getVal (Rec ys) l

    -- check whether a record contain the field
    hasField :: Term -> String -> Bool
    hasField (Rec []) _             = False
    hasField (Rec ((l1, t1) : ys)) l 
       | l1 == l                    = True
       | otherwise                  = hasField (Rec ys) l

    -- evaluate a record 
    evalRecord :: Term -> Maybe Term 
    evalRecord (Rec [])               = Just $ Rec []
    evalRecord (Rec ((l1, v1) : ys)) 
      | isVal v1                      = (`addEntry` (l1, v1)) <$> evalRecord (Rec ys)
      | otherwise                     = case evaluate' v1 of 
                                          Just res     -> (`addEntry` (l1, res)) <$> evalRecord (Rec ys)
                                          Nothing      -> Nothing

    -- add new entry to the record
    addEntry :: Term -> Entry -> Term
    addEntry (Rec ls) newElem = Rec (newElem : ls)

    -- small-step evaluation
    evaluate' :: Term -> Maybe Term
    evaluate' t = case t of
      -- Arithmetic
      Pred Zero                           -> Just Zero                                    -- (E-PREDZERO)
      Pred (Succ nv) | isNumeric nv       -> Just nv                                      -- (E-PREDSUCC)
      IsZero Zero                         -> Just Tru                                     -- (E-ISZEROZERO)
      IsZero (Succ nv) | isNumeric nv     -> Just Fls                                     -- (E-ISZEROSUCC)
      IsZero t1 | not (isNumeric t1)      -> IsZero <$> evaluate' t1                      -- (E-ISZERO)
      Succ t1                             -> Succ <$> evaluate' t1                        -- (E-SUCC)
      Pred t1                             -> Pred <$> evaluate' t1                        -- (E-PRED)

      -- Conditional
      If Tru t2 t3                        -> Just t2                                      -- (E-IFTRUE)
      If Fls t2 t3                        -> Just t3                                      -- (E-IFFALSE)
      If t1 t2 t3                         -> (\t1' -> If t1' t2 t3) <$> evaluate' t1      -- (E-IF)

      -- Cast
      Cast (Iden _) v | isUncoercedVal v  -> Just v                                       -- (E-CID)
      App (Cast (Func c d) v1) v2 
        | isUncoercedVal v1 && isVal v2   -> Cast d <$> evaluate' (App v1 (Cast c v2))    -- (E-CAPP)
      Cast c (Cast d t)                   -> Cast (combineCoercions c d) <$> evaluate' t  -- (E-CCAST)

      -- Application
      App (Lambda _ t1 _) v2 | isVal v2   -> Just $ subsFromTop v2 t1                     -- (E-APPABS)
      App v1 t2 | isVal v1                -> App v1 <$> evaluate' t2                      -- (E-APP2)
      App t1 t2                           -> (`App` t2) <$> evaluate' t1                  -- (E-APP1)

      -- Records
      Proj (Rec ls) l | isVal (Rec ls)    -> getVal (Rec ls) l                            -- (E-PROJRCD)
      Proj (Rec ls) l                     -> (`Proj` l) <$> evaluate' (Rec ls)            -- (E-PROJ)
      Rec ls | not (isVal (Rec ls))       -> evalRecord t                                 -- (E-RCD)
                       
      -- No rules applied
      _                                   -> Nothing                                      -- "Stuck"

    -- big-step evaluation
    -- (apply evaluate' repeatedly until a value is reached or we're left with an expression
    -- that cannot be evaluated further)
    evaluateToValue :: Term -> Term
    evaluateToValue x = fromMaybe x (evaluateToValue <$> evaluate' x)
    
    -- evaluate a term
    evaluate :: Term -> Maybe Term
    evaluate t = case evaluateToValue t of
      res | isVal res -> Just res
          | otherwise -> Nothing -- term is "stuck"