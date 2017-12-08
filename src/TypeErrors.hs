module TypeErrors where 
    
    import Syntax
    import Types
    
    data TypeError 
      = NotBound Term -- free variable
      | Difference Type Type
      | Mismatch Type Type
      | NotBool Term Type
      | NotNat Type
      | NotFunction Term
      | NotRecord Term
      | NotFound String 
      | IllTyped
      deriving (Eq, Show)