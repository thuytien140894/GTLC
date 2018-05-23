module Subtype 
    ( isSubtype 
    , sortFields
    ) where 

    import Type

    import Data.Function (on)
    import qualified Data.List as List (sortBy)

    -- | Check the subtype relation of two records.
    isRcdSubtype :: [TEntry] -> [TEntry] -> Bool
    isRcdSubtype s t 
        | widthS >= widthT = sortedS `isSubset` sortedT
        | otherwise        = False
      where 
        (widthS, widthT)   = (length s, length t)
        (sortedS, sortedT) = (sortFields s, sortFields t)
                        
    -- | Sort an array of (field, type) by the field names.                                   
    sortFields :: [TEntry] -> [TEntry]
    sortFields = List.sortBy (compare `on` fst)

    -- | Check if one record is a subset of another.
    -- The first list is always at least longer than the second one.
    -- This method combines both S-RCDWIDTH and S-RCDDEPTH.
    isSubset :: [TEntry] -> [TEntry] -> Bool
    isSubset _ []   = True
    isSubset [] _   = False
    isSubset ((l1, s1) : xs) ((l2, s2) : ys) 
        | l1 == l2  = s1 `isSubtype` s2 && xs `isSubset` ys
        | otherwise = xs `isSubset` ((l2, s2) : ys)
    
    -- | Check if a type is a subtype of another.
    isSubtype :: Type -> Type -> Bool
    isSubtype s Top                   = True                  -- ^ (S-TOP)
    isSubtype s t 
        | s == t                      = True                  -- ^ (S-REFL)
    isSubtype (Arr s1 s2) (Arr t1 t2) = t1 `isSubtype` s1 
                                        && s2 `isSubtype` t2  -- ^ (S-ARROW)
    isSubtype (TRec s) (TRec t)       = s `isRcdSubtype` t    -- ^ (S-RCD)
    isSubtype _ _                     = False 

    -- | Subtyping rules for UD blame assignment.
    isSubtypeUD :: Type -> Type -> Bool
    isSubtypeUD s t 
        | s == t                        = True
    isSubtypeUD Boolean Dyn             = True                                      
    isSubtypeUD Nat Dyn                 = True                                      
    isSubtypeUD (Arr s1 s2) Dyn         = isSubtypeUD (Arr s1 s2) (Arr Dyn Dyn) 
    isSubtypeUD (Arr s1 s2) (Arr t1 t2) = t1 `isSubtypeUD` s1 && s2 `isSubtypeUD` t2
    isSubtypeUD _ _                     = False     