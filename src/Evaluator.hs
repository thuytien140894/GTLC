module Evaluator ( 
  evaluate
  ) where

  import Syntax
  import Coercion
  import TypeChecker
  import Errors
  import Types
  import Utils
  
  import Data.Maybe
  import Control.Applicative
  import Data.Functor
  import Data.Either
  import Data.Map as Map 

  -- determine if a term is a value
  isVal :: Term -> Bool
  isVal t = isUncoercedVal t || isCoercedVal t
  
  -- determine if a term is a numeric value
  isNumeric :: Term -> Bool
  isNumeric t = case t of 
    Zero                   -> True
    Succ t'                -> isNumeric t'
    _                      -> False

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
    Loc _                 -> True
    Blame _               -> True
    _                     -> False

  -- determine if a term is a coerced value
  isCoercedVal :: Term -> Bool
  isCoercedVal (Cast c v)  
    | isUncoercedVal v && isRegular c    = True 
  isCoercedVal _                         = False
  
  -- perform substitution from the beginning
  subsFromTop :: Term -> Term -> Term
  subsFromTop s t = shift 0 (-1) (subs 0 (shift 0 1 s) t)

  -- get the value for the specified field
  getVal :: Term -> String -> Either RuntimeError Term
  getVal (Rec []) _               = Left Stuck
  getVal (Rec ((l1, t1) : ys)) l 
    | l1 == l                     = Right t1
    | otherwise                   = Rec ys `getVal` l

  -- evaluate a record 
  evalRecord :: (Term, StoreEnv) -> Either RuntimeError (Term, StoreEnv) 
  evalRecord (Rec [], store)              = Right (Rec [], store)
  evalRecord (Rec ((l1, t1) : ys), store) 
    | isVal t1                            = do 
                                              (rd, store') <- evalRecord (Rec ys, store)
                                              Right (rd `addEntry` (l1, t1), store')
    | otherwise                           = do 
                                              (t1', store') <- evaluate' (t1, store) 
                                              (rd, store'') <- evalRecord (Rec ys, store')
                                              Right (rd `addEntry` (l1, t1'), store'')

  -- remove an enclosing coercion from a value 
  -- if the run-time type matches the target type
  unbox :: Term -> StoreEnv -> Either RuntimeError Term
  unbox (Cast c v) store = case typeOf v store of 
    srcTy | srcTy `isConsistent` cstTy  -> Right v
          | otherwise                   -> Left CastError
          where cstTy = snd $ getCoercionTypes c

  -- small-step evaluation
  evaluate' :: (Term, StoreEnv) -> Either RuntimeError (Term, StoreEnv)
  evaluate' (t, store) = case t of
    -- Blame 
    IsZero (Blame l)                   -> Right (Blame l, store)                      -- (E-BISZERO)
    Succ (Blame l)                     -> Right (Blame l, store)                      -- (E-BSUCC)
    Pred (Blame l)                     -> Right (Blame l, store)                      -- (E-BPRED)
    If (Blame l) t1 t2                 -> Right (Blame l, store)                      -- (E-BIF)
    App (Blame l) _                    -> Right (Blame l, store)                      -- (E-BAPP1)
    App _ (Blame l)                    -> Right (Blame l, store)                      -- (E-BAPP2)
    Cast _ (Blame l)                   -> Right (Blame l, store)                      -- (E-BCAST)

    -- Arithmetic
    Pred Zero                          -> Right (Zero, store)                         -- (E-PREDZERO)
    Pred (Succ nv) 
      | isNumeric nv                   -> Right (nv, store)                           -- (E-PREDSUCC)
    Pred t'                            -> do                                          -- (E-PRED)
                                            (t'', store') <- evaluate' (t', store)
                                            Right (Pred t'', store')
    IsZero Zero                        -> Right (Tru, store)                          -- (E-ISZEROZERO)
    IsZero (Succ nv) 
      | isNumeric nv                   -> Right (Fls, store)                          -- (E-ISZEROSUCC)
    IsZero t'                          -> do                                          -- (E-ISZERO)
                                            (t'', store') <- evaluate' (t', store)
                                            Right (IsZero t'', store')
    Succ t'                            -> do                                          -- (E-SUCC)
                                            (t'', store') <- evaluate' (t', store)
                                            Right (Succ t'', store')

    -- Conditional
    If Tru t2 t3                       -> Right (t2, store)                           -- (E-IFTRUE)
    If Fls t2 t3                       -> Right (t3, store)                           -- (E-IFFALSE)
    If t1 t2 t3                        -> do                                          -- (E-IF)
                                            (t1', store') <- evaluate' (t1, store)
                                            Right (If t1' t2 t3, store')

    -- Cast
    Cast c t' 
      | not (isVal t')                 -> do                                          -- (E-CCAST)
                                            (t'', store') <- evaluate' (t', store)
                                            Right (Cast c t'', store')
    Cast c (Cast d u)                  -> let t' = Seq d c `Cast` u                   -- (E-CCMP)
                                          in Right (t', store)             
    Cast (Iden _) u                    -> Right (u, store)                            -- (E-CID)
    Cast (Fail _ _ l) u                -> Right (Blame l, store)                      -- (E-CFAIL)
    Cast c u 
      | isNormalized c                 -> do                                          -- (E-CGROUND)
                                            t' <- unbox t store
                                            Right (t', store)                               
    Cast c u                           -> let t' = reduceCoercion c `Cast` u          -- (E-CSTEP)
                                          in Right (t', store)    
    Deref (Cast (CRef c d) (Loc l))    -> let t' = Cast d $ Deref (Loc l)             -- (E-CDEREF)
                                          in Right (t', store)       
    Assign (Cast (CRef c d) (Loc l)) v                                                -- (E-CASSIGN)
      | isVal v                        -> let t' = Cast d $ Loc l `Assign` Cast c v
                                          in Right (t', store)    
    App (Cast (Func c d) u) v 
      | isUncoercedVal u && 
        isVal v                        -> let t' = Cast d $ u `App` Cast c v          -- (E-CAPP)
                                          in Right (t', store)                        

    -- Reference
    Ref v     
      | isVal v                        -> Right $ allocate store v                    -- (E-REFV)                 
    Ref t'                             -> do                                          -- (E-REFV)
                                            (t'', store') <- evaluate' (t', store)
                                            Right (Ref t'', store')
    Deref (Loc l)                      -> case store `lookUp` l of                    -- (E-DEREFLOC)
                                            Just (Store (v, _)) -> Right (v, store)
                                            Nothing              -> Left $ InvalidRef l

    Deref t'                           -> do                                          -- (E-DEREF)
                                            (t'', store') <- evaluate' (t', store)
                                            Right (Deref t'', store')
    Assign (Loc l) v
      | isVal v                        -> updateStore store l v                       -- (E-ASSIGN)
    Assign v1 t2 
      | isVal v1                       -> do                                          -- (E-ASSIGN2)
                                            (t2', store') <- evaluate' (t2, store)
                                            Right (Assign v1 t2', store')
    Assign t1 t2                       -> do                                          -- (E-ASSIGN1)
                                            (t1', store') <- evaluate' (t1, store)
                                            Right (Assign t1' t2, store')

    -- Application
    App (Lambda _ t1 _) v2 
      | isVal v2                       -> Right (subsFromTop v2 t1, store)            -- (E-APPABS)
    App v1 t2 
      | isVal v1                       -> do                                          -- (E-APP2)
                                            (t2', store') <- evaluate' (t2, store)
                                            Right (App v1 t2', store')              
    App t1 t2                          -> do                                          -- (E-APP1)
                                            (t1', store') <- evaluate' (t1, store)
                                            Right (App t1' t2, store')              

    -- Records
    Proj (Rec ls) l 
      | isVal $ Rec ls                 -> do                                          -- (E-PROJRCD)
                                            t' <- Rec ls `getVal` l                 
                                            Right (t', store)
    Proj (Rec ls) l                    -> do                                          -- (E-PROJ)
                                            (t', store') <- evaluate' (Rec ls, store)
                                            Right (Proj t' l, store')            
    Rec ls 
      | not $ isVal $ Rec ls           -> evalRecord (t, store)                       -- (E-RCD)
                      
    -- No rules applied
    _                                  -> Left Stuck                                  -- "Stuck"

  -- big-step evaluation
  -- (apply evaluate' repeatedly until a value is reached or we're left with an expression
  -- that cannot be evaluated further)
  evaluateToValue :: (Term, StoreEnv) -> Either RuntimeError (Term, StoreEnv)
  evaluateToValue x = case evaluate' x of
    Right res  -> evaluateToValue res
    Left Stuck -> Right x 
    Left err   -> Left err
  
  -- evaluate a term
  evaluate :: Term -> Either RuntimeError Term
  evaluate t = case evaluateToValue (t, StoreEnv Map.empty) of
    Right (res, _) 
      | isUncoercedVal res -> Right res
      | otherwise          -> Left Stuck -- term is "stuck"
    Left err               -> Left err