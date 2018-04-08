module ParseHelper where 

  import Syntax
  import Types

  import Data.Maybe
  import Data.List (elemIndex)

  -- correct the bruijn index for a bound variable
  -- this function is called when parsing a Lambda term 
  fixBinding :: Term -> String -> Int -> Term
  fixBinding t x b = case t of
    Var _ ty id | id == x   -> Var b ty id
    Succ t'                 -> Succ (fixBinding t' x b)
    Pred t'                 -> Pred (fixBinding t' x b)
    IsZero t'               -> IsZero (fixBinding t' x b)
    Lambda ty t' ctx        -> Lambda ty (fixBinding t' x (b + 1)) ctx
    App t1 t2               -> App (fixBinding t1 x b) (fixBinding t2 x b)
    _                       -> t

  -- fix Bruijn indices for free variables
  fixFreeBinding :: Term -> [String] -> [String] -> Term
  fixFreeBinding t freeVars boundVars = case t of
    Var _ ty id 
      | id `elem` freeVars  -> Var (getBruijnIndex id freeVars boundVars) ty id
    Succ t'                 -> Succ (fixFreeBinding t' freeVars boundVars)
    Pred t'                 -> Pred (fixFreeBinding t' freeVars boundVars)
    IsZero t'               -> IsZero (fixFreeBinding t' freeVars boundVars)
    Lambda ty t' ctx        -> Lambda ty (fixFreeBinding t' freeVars boundVars) ctx
    App t1 t2               -> App (fixFreeBinding t1 freeVars boundVars) (fixFreeBinding t2 freeVars boundVars)
    _                       -> t

  -- update the typing environment for nested lambdas when new bound variables are introduced
  updateVarType :: Term -> String -> Type -> Term
  updateVarType t x ty = case t of 
    Var k _ id | id == x       -> Var k ty id
    Succ t'                    -> Succ (updateVarType t' x ty)
    Pred t'                    -> Pred (updateVarType t' x ty)
    IsZero t'                  -> IsZero (updateVarType t' x ty)
    Lambda ty t' ctx           -> Lambda ty (updateVarType t' x ty) ctx 
    App t1 t2                  -> App (updateVarType t1 x ty) (updateVarType t2 x ty)
    _                          -> t

  -- get the bruijn index for a free variable
  getBruijnIndex :: String -> [String] -> [String] -> Int
  getBruijnIndex id freeVars boundVars = fromJust freeIndex + length boundVars
    where freeIndex  = elemIndex id (reverse freeVars)

  -- retrieve the binding context of an abstraction
  getBoundVar :: Term -> [String]
  getBoundVar t = case t of
    Lambda _ _ ctx -> ctx
    Succ t'        -> getBoundVar t'
    Pred t'        -> getBoundVar t'
    IsZero t'      -> getBoundVar t'
    App t1 t2      -> getBoundVar t1 ++ getBoundVar t2
    _              -> []

  -- find free variables
  getFreeVar :: Term -> [String] -> [String]
  getFreeVar t boundVars = case t of
    Lambda _ t1 _        -> getFreeVar t1 boundVars
    App t1 t2            -> getFreeVar t1 boundVars ++ getFreeVar t2 boundVars
    Var _ _ id           -> if id `elem` boundVars then [] else [id]
    _                    -> []  

  -- application
  apply :: Term -> Term -> Term
  apply Unit t2 = t2
  apply t1 t2   = App t1 t2

  -- recursively apply terms from the left
  applyFromLeft :: [Term] -> Term
  applyFromLeft = foldl apply Unit

  -- "arrow" two types
  arrow :: Type -> Type -> Type
  arrow t2 TUnit = t2
  arrow t1 t2    = Arr t1 t2

  -- recursively "arrow" types from the right
  arrowFromRight :: [Type] -> Type
  arrowFromRight = foldr arrow TUnit