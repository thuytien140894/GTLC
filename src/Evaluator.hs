module Evaluator ( 
  evaluate
  ) where

  import Syntax
  import Coercion
  import TypeChecker
  import Errors
  
  import Data.Maybe
  import Control.Applicative
  import Data.Functor
  import Data.Either 
  
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
    Blame _               -> True
    _                     -> False

  -- determine if a term is a coerced value
  isCoercedVal :: Term -> Bool
  isCoercedVal (Cast c v)  
    | isUncoercedVal v && isRegular c    = True 
  isCoercedVal _                         = False

  -- determine if a term is a value
  isVal :: Term -> Bool
  isVal t = isUncoercedVal t || isCoercedVal t
  
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
  shift a b t = case t of 
    Var k ty id      -> if k < a then t else Var (k + b) ty id
    Cast c t'        -> Cast c $ shift a b t'
    Succ t'          -> Succ $ shift a b t'
    Pred t'          -> Pred $ shift a b t'
    IsZero t'        -> IsZero $ shift a b t'
    Lambda ty t' ctx -> Lambda ty (shift (a + 1) b t') ctx 
    App t1 t2        -> shift a b t1 `App` shift a b t2
    _                -> t -- t is a constant
    
  -- perform substitution given a variable with bruijn index j, a body s, and a term t
  subs :: Int -> Term -> Term -> Term 
  subs j s t = case t of 
    Var k ty id      -> if k == j then s else t
    Cast c t'        -> Cast c $ subs j s t'
    Succ t'          -> Succ $ subs j s t'
    Pred t'          -> Pred $ subs j s t'
    IsZero t'        -> IsZero $ subs j s t'
    Lambda ty t' ctx -> Lambda ty (subs (j + 1) (shift 0 1 s) t') ctx
    App t1 t2        -> subs j s t1 `App` subs j s t2
    _                -> t -- t is a constant
  
  -- perform substitution from the beginning
  subsFromTop :: Term -> Term -> Term
  subsFromTop s t = shift 0 (-1) (subs 0 (shift 0 1 s) t)

  -- get the value for the specified field
  getVal :: Term -> String -> Either RuntimeError Term
  getVal (Rec []) _               = Left Stuck
  getVal (Rec ((l1, t1) : ys)) l 
    | l1 == l                     = Right t1
    | otherwise                   = Rec ys `getVal` l

  -- check whether a record contain the field
  hasField :: Term -> String -> Bool
  hasField (Rec []) _              = False
  hasField (Rec ((l1, t1) : ys)) l 
      | l1 == l                    = True
      | otherwise                  = Rec ys `hasField` l

  -- evaluate a record 
  evalRecord :: Term -> Either RuntimeError Term 
  evalRecord (Rec [])               = Right $ Rec []
  evalRecord (Rec ((l1, v1) : ys)) 
    | isVal v1                      = (`addEntry` (l1, v1)) <$> evalRecord (Rec ys)
    | otherwise                     = case evaluate' v1 of 
                                        Right res     -> (`addEntry` (l1, res)) <$> evalRecord (Rec ys)
                                        Left err      -> Left err

  -- add new entry to the record
  addEntry :: Term -> Entry -> Term
  addEntry (Rec ls) newElem = Rec (newElem : ls)

  -- remove an enclosing coercion from a value 
  -- if the run-time type matches the target type
  unbox :: Term -> Either RuntimeError Term
  unbox (Cast c v) = case typeOf v of 
    Right srcTy 
      | srcTy `isConsistent` cstTy  -> Right v
      | otherwise                   -> Left CastError
      where cstTy = snd $ getCoercionTypes c
    Left err                        -> Left $ TError err

  -- small-step evaluation
  evaluate' :: Term -> Either RuntimeError Term
  evaluate' t = case t of
    -- Blame 
    IsZero (Blame l)                    -> Right $ Blame l                               -- (E-BISZERO)                                       
    Succ (Blame l)                      -> Right $ Blame l                               -- (E-BSUCC)
    Pred (Blame l)                      -> Right $ Blame l                               -- (E-BPRED)
    If (Blame l) t2 t1                  -> Right $ Blame l                               -- (E-BIF)
    App (Blame l) _                     -> Right $ Blame l                               -- (E-BAPP1)
    App _ (Blame l)                     -> Right $ Blame l                               -- (E-BAPP2)
    Cast _ (Blame l)                    -> Right $ Blame l                               -- (E-BCAST)

    -- Arithmetic
    Pred Zero                           -> Right Zero                                    -- (E-PREDZERO)
    Pred (Succ nv) 
      | isNumeric nv                    -> Right nv                                      -- (E-PREDSUCC)
    IsZero Zero                         -> Right Tru                                     -- (E-ISZEROZERO)
    IsZero (Succ nv) 
      | isNumeric nv                    -> Right Fls                                     -- (E-ISZEROSUCC)
    IsZero t1 
      | not (isNumeric t1)              -> IsZero <$> evaluate' t1                       -- (E-ISZERO)
    Succ t1                             -> Succ <$> evaluate' t1                         -- (E-SUCC)
    Pred t1                             -> Pred <$> evaluate' t1                         -- (E-PRED)

    -- Conditional
    If Tru t2 t3                        -> Right t2                                      -- (E-IFTRUE)
    If Fls t2 t3                        -> Right t3                                      -- (E-IFFALSE)
    If t1 t2 t3                         -> (\t1' -> If t1' t2 t3) <$> evaluate' t1       -- (E-IF)

    -- Cast
    Cast c t 
      | not (isVal t)                   -> Cast c <$> evaluate' t                        -- (E-CCAST)
    Cast c (Cast d v)                   -> Right $ Cast (Seq d c) v                      -- (E-CCOMB)
    Cast (Iden _) v                     -> Right v                                       -- (E-CID)
    Cast (Fail _ _ l) v                 -> Right $ Blame l                               -- (E-CFAIL)
    Cast c v 
      | isNormalized c                  -> unbox t                                       -- (E-CGROUND)
    Cast c v                            -> Right $ Cast (reduceCoercion c) v             -- (E-CSTEP)
    App (Cast (Func c d) v1) v2 
      | isUncoercedVal v1 && 
        isVal v2                        -> Right $ Cast d (App v1 $ Cast c v2)           -- (E-CAPP)

    -- Application
    App (Lambda _ t1 _) v2 
      | isVal v2                        -> Right $ subsFromTop v2 t1                     -- (E-APPABS)
    App v1 t2 
      | isVal v1                        -> App v1 <$> evaluate' t2                       -- (E-APP2)
    App t1 t2                           -> (`App` t2) <$> evaluate' t1                   -- (E-APP1)

    -- Records
    Proj (Rec ls) l 
      | isVal $ Rec ls                  -> Rec ls `getVal` l                             -- (E-PROJRCD)
    Proj (Rec ls) l                     -> (`Proj` l) <$> evaluate' (Rec ls)             -- (E-PROJ)
    Rec ls 
      | not $ isVal $ Rec ls            -> evalRecord t                                  -- (E-RCD)
                      
    -- No rules applied
    _                                   -> Left Stuck                                    -- "Stuck"

  -- big-step evaluation
  -- (apply evaluate' repeatedly until a value is reached or we're left with an expression
  -- that cannot be evaluated further)
  evaluateToValue :: Term -> Either RuntimeError Term
  evaluateToValue x = case evaluate' x of
    Right res  -> evaluateToValue res
    Left Stuck -> Right x 
    Left err   -> Left err
  
  -- evaluate a term
  evaluate :: Term -> Either RuntimeError Term
  evaluate t = case evaluateToValue t of
    Right res 
      | isUncoercedVal res -> Right res
      | otherwise          -> Left Stuck -- term is "stuck"
    Left err               -> Left err
      
      