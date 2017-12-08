module Subtype where 

    import Types

    import Data.List (sortBy)
    import Data.Function (on)

    -- check the subtype relation of two records
    isRcdSubtype :: [TEntry] -> [TEntry] -> Bool
    isRcdSubtype s t 
        | widthS >= widthT  = sortedS `isSubset` sortedT
        | otherwise         = False
        where (widthS, widthT)   = (length s, length t)
              (sortedS, sortedT) = (sortFields s, sortFields t)
                        
    -- sort an array of (field, type) by the field names                                   
    sortFields :: [TEntry] -> [TEntry]
    sortFields = sortBy (compare `on` fst)

    -- check if one record is a subset of another
    -- the first list is always at least longer than the second one
    isSubset :: [TEntry] -> [TEntry] -> Bool
    isSubset _ []                               = True
    isSubset [] _                               = False
    isSubset ((l1, ty1) : xs) ((l2, ty2) : ys) 
        | l1 == l2                              = ty1 `isSubtype` ty2 && xs `isSubset` ys
        | otherwise                             = xs `isSubset` ((l2, ty2) : ys)
    
    isSubtype :: Type -> Type -> Bool
    isSubtype _ Top                     = True
    isSubtype s t | s == t              = True
    isSubtype (Arr s1 s2) (Arr t1 t2)   = t1 `isSubtype` s1 && s2 `isSubtype` t2
    isSubtype (TRec s) (TRec t)         = s `isRcdSubtype` t
    isSubtype _ _                       = False 
                             