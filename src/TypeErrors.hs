module TypeErrors where 
    
    import Syntax
    import Types
    
    data TypeError 
      = NotBound Term 
      | Difference Type Type
      | Mismatch Type Type
      | NotBool Type
      | NotNat Type
      | NotFunction Term
      | NotRecord Term
      | NotFound String 
      | IllTyped
      deriving (Eq, Show)