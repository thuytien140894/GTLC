module ParseHelper where 

    import Utils
    import Syntax
    import Types

    import Data.Maybe
    import Data.List

    -- correct the bruijn index for each variable
    -- this function is called when parsing a Lambda term 
    fixBinding :: Term -> [(String, Type)] -> [String] -> Term
    fixBinding t boundVars freeVars = case t of
      Var _ id                -> Var (getBruijnIndex id boundVars freeVars) id
      Lambda ty t1 localBound -> Lambda ty (fixBinding t1 boundVars freeVars) localBound
      App t1 t2               -> App (fixBinding t1 boundVars freeVars) (fixBinding t2 boundVars freeVars)
      _                       -> t

    -- update the typing environment for nested lambdas when new bound variables are introduced
    updateTypingEnv :: Term -> [(String, Type)] -> Term
    updateTypingEnv t env = case t of 
      Lambda ty t' _      -> Lambda ty (updateTypingEnv t' env) env 
      App t1 t2           -> App (updateTypingEnv t1 env) (updateTypingEnv t2 env)
      _                   -> t

    -- get the bruijn index for a variable with a given identifier and its binding context
    getBruijnIndex :: String -> [(String, Type)] -> [String] -> Int
    getBruijnIndex id boundVars freeVars 
      | isNothing boundIndex = fromJust freeIndex + length boundVars
      | otherwise            = fromJust boundIndex
      where boundIndex = indexOf id (reverse boundVars) 0
            freeIndex  = elemIndex id (reverse freeVars)

    -- retrieve the binding context of an abstraction
    getBoundVar :: Term -> [(String, Type)]
    getBoundVar t = case t of
      Lambda _ _ ctx -> ctx
      _              -> []

    -- find free variables
    getFreeVar :: Term -> [(String, Type)] -> [String]
    getFreeVar t boundVars = case t of
      Lambda _ t1 _        -> getFreeVar t1 boundVars
      App t1 t2            -> getFreeVar t1 boundVars ++ getFreeVar t2 boundVars
      Var _ id             -> if id `isElem` boundVars then [] else [id]
      _                    -> []