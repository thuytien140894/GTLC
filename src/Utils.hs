module Utils where
    
    import Syntax

    import Data.Maybe
    import Data.List

    -- retrieve the binding context of an abstraction
    getBindingContext :: Term -> [String]
    getBindingContext t = case t of
      Lambda _ _ ctx -> ctx
      _              -> []

    -- correct the bruijn index for each variable
    -- this function is called when parsing a Lambda term 
    fixBinding :: Term -> [String] -> [String] -> Term
    fixBinding t boundVars freeVars = case t of
      Lambda ty t1 localBound -> Lambda ty (fixBinding t1 boundVars freeVars) localBound
      App t1 t2               -> App (fixBinding t1 boundVars freeVars) (fixBinding t2 boundVars freeVars)
      Var _ id                -> Var (getBruijnIndex id boundVars freeVars) id
      _                       -> t

    -- get the bruijn index for a variable with a given identifier and its binding context
    getBruijnIndex :: String -> [String] -> [String] -> Int
    getBruijnIndex id boundVars freeVars 
      | isNothing boundIndex = fromJust freeIndex + length boundVars
      | otherwise            = fromJust boundIndex
      where boundIndex = elemIndex id (reverse boundVars)
            freeIndex = elemIndex id (reverse freeVars)

    -- find free variables
    findFreeVar :: Term -> [String] -> [String]
    findFreeVar t boundVars = case t of
      Lambda _ t1 _        -> findFreeVar t1 boundVars
      App t1 t2            -> findFreeVar t1 boundVars ++ findFreeVar t2 boundVars
      Var _ id             -> if id `isElem` boundVars then [] else [id]
      _                    -> []
        
    -- check if an element is in a list
    isElem :: String -> [String] -> Bool
    isElem x list = case list of 
      []             -> False
      y : ys         -> if x == y then True else isElem x ys
