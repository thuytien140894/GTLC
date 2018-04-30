module Error where 
    
    import Syntax
    import Type
    
    -- | Blame result.
    data BlameRes = BlameRes Cause Term 
                    deriving (Eq, Show)

    -- | The origin of a coercion.
    data Cause = FunArg     -- ^ Coercion for function argument
               | FunRet     -- ^ Coercion for function return value
               | RefRead    -- ^ Coercion for reference read
               | RefWrite   -- ^ Coercion for reference write
               | Function   -- ^ Coercion to a function type
               | Reference  -- ^ Coercion to a reference type
               | None       -- ^ Singular coerion
                 deriving (Eq, Show)

    -- | Errors during typechecking.
    data TypeError = NotBound Term                  -- ^ Not in scope variable
                   | Difference Type Type           -- ^ Different branch type
                   | FunMismatch Type Type Term     -- ^ Different function argument type
                   | NotBool Type                   -- ^ Bool is expected
                   | NotNat Type                    -- ^ Nat is expected
                   | NotFunction Term               -- ^ Not a function
                   | IllegalAssign Term             -- ^ Assigning to a non-reference
                   | AssignMismatch Type Type Term  -- ^ Different types on two sides of an assignment
                   | IllegalDeref Term              -- ^ Dereference a non-reference
                   | NotRecord Term                 -- ^ Not a record term
                   | InvalidLabel String            -- ^ Nonexistent record label
                     deriving (Eq, Show)

    -- | Errors during evaluation.
    data RuntimeError = CastError Type Type BlameRes  -- ^ Cast error 
                      | Blame Type Type Label Term    -- ^ Blame assignment
                      | InvalidRef Int                -- ^ Nonexistent reference
                      | Stuck                         -- ^ Stuck evaluation
                        deriving (Eq, Show)