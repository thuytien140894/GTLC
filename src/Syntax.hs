{-# Language PatternSynonyms #-}

module Syntax where

    import Type

    -- | Entry in a record.
    type Entry = (String, Term)

    -- | Blame label.
    newtype Label = Label Int
                    deriving (Eq, Show)

    -- | Fun!
    pattern FuncInj :: Coercion 
    pattern FuncInj = Inject (Arr Dyn Dyn)

    -- | Fun?
    pattern FuncProj :: Label -> Coercion 
    pattern FuncProj l = Project (Arr Dyn Dyn) l

    -- | Ref!
    pattern RefInj :: Coercion
    pattern RefInj = Inject (TRef Dyn)

    -- | Ref?
    pattern RefProj :: Label -> Coercion
    pattern RefProj l = Project (TRef Dyn) l
    
    -- | Expressions
    data Term = Unit                       -- ^ unit 
              | Zero                       -- ^ 0
              | Tru                        -- ^ true
              | Fls                        -- ^ false
              | Var Int Type String        -- ^ Variable {bruijn index, type, name}
              | If Term Term Term          -- ^ if t then t else t
              | Succ Term                  -- ^ succ t 
              | Pred Term                  -- ^ pred t
              | IsZero Term                -- ^ iszero t
              | Rec [Entry]                -- ^ Record
              | Proj Term String           -- ^ Projection
              | Lambda Type Term [String]  -- ^ Abstraction {type, body, bound variables}
              | App Term Term              -- ^ Application 
              | Ref Term                   -- ^ Reference creation
              | Deref Term                 -- ^ Dereference (!t)
              | Loc Int                    -- ^ Store location
              | Assign Term Term           -- ^ Assignment (t := t)
              | Cast Coercion Term         -- ^ Coercion (<c> t)
                deriving (Eq, Show) 
    
    -- | Coercions
    data Coercion = Iden Type               -- ^ Identity 
                  | Project Type Label      -- ^ Projection (B?)
                  | Inject Type             -- ^ Injection (B!)
                  | CRef Coercion Coercion  -- ^ Reference coercion
                  | Func Coercion Coercion  -- ^ Function coercion
                  | Seq Coercion Coercion   -- ^ Coercion sequence (c;c)
                  | Fail Type Type Label    -- ^ Fail
                    deriving (Eq, Show)