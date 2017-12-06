module TypeErrors where 
    
    import Syntax
    import Types
    
    data TypeError 
      = NotBound Term -- free variable
      | Difference Term Term
      | Mismatch Term Type
      | NotFunction Term
      | IllTyped
      deriving (Eq, Show)