module Syntax where
    -- declaring a new data type
    -- the new values of the type are called constructors
    -- values with arguments are called constructor functions, which are themselves fully evaluated
    data Term 
      = Tr
      | Fl
      | Var Int
      | Lambda Term -- abstraction
      | App Term Term 
      deriving (Eq, Show) -- make the data type an instance of these built-in classes

-- the basic types in Haskell are Bool, Char, Int, String, Int, Integer, Float