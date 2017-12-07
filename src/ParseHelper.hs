module ParseHelper where 

    import Syntax
    import Types

    import Data.Maybe
    import Data.List 

    -- correct the bruijn index for each variable
    -- this function is called when parsing a Lambda term 
    fixBinding :: Term -> [String] -> [String] -> Term
    fixBinding t boundVars freeVars = case t of
      Var _ ty id             -> Var (getBruijnIndex id boundVars freeVars) ty id
      Lambda ty t1 localBound -> Lambda ty (fixBinding t1 boundVars freeVars) localBound
      App t1 t2               -> App (fixBinding t1 boundVars freeVars) (fixBinding t2 boundVars freeVars)
      _                       -> t

    -- update the typing environment for nested lambdas when new bound variables are introduced
    updateVarType :: Term -> String -> Type -> Term
    updateVarType t varName ty = case t of 
      Var k _ id            -> if id == varName then Var k ty id else t
      Lambda ty t' ctx      -> Lambda ty (updateVarType t' varName ty) ctx 
      App t1 t2             -> App (updateVarType t1 varName ty) (updateVarType t2 varName ty)
      _                     -> t

    -- get the bruijn index for a variable with a given identifier and its binding context
    getBruijnIndex :: String -> [String] -> [String] -> Int
    getBruijnIndex id boundVars freeVars 
      | isNothing boundIndex = fromJust freeIndex + length boundVars
      | otherwise            = fromJust boundIndex
      where boundIndex = elemIndex id (reverse boundVars)
            freeIndex  = elemIndex id (reverse freeVars)

    -- retrieve the binding context of an abstraction
    getBoundVar :: Term -> [String]
    getBoundVar t = case t of
      Lambda _ _ ctx -> ctx
      _              -> []

    -- find free variables
    getFreeVar :: Term -> [String] -> [String]
    getFreeVar t boundVars = case t of
      Lambda _ t1 _        -> getFreeVar t1 boundVars
      App t1 t2            -> getFreeVar t1 boundVars ++ getFreeVar t2 boundVars
      Var _ _ id           -> if id `isElem` boundVars then [] else [id]
      _                    -> []

    -- check if an element is in a list
    isElem :: String -> [String] -> Bool
    isElem x list = case list of 
      []            -> False
      y : ys        -> if x == y then True else isElem x ys

    -- merge all the entries into one record
    merge :: [Term] -> [(String, Term)]
    merge ls = case ls of 
      []               -> []
      Rec entry : ys   -> entry ++ merge ys    