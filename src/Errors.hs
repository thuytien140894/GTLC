module Errors where 
    
  import Syntax
  import Types
  
  data TypeError 
    = NotBound Term 
    | Difference Type Type
    | Mismatch Type Type
    | NotBool Type
    | NotNat Type
    | NotFunction Term
    | NotRef Term
    | NotRecord Term
    | NotFound String
    | IllegalCast Type Type
    deriving (Eq, Show)

  data RuntimeError 
    = CastError
    | InvalidRef Int
    | Stuck
    deriving (Eq, Show)