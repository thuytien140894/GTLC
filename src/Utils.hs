module Utils where
    
    import Types

    import Data.Maybe

    -- check if an element is in a list
    isElem :: String -> [(String, Type)] -> Bool
    isElem x list = case list of 
      []                   -> False
      (id, ty) : ys        -> if x == id then True else isElem x ys

    -- find the index of an occurence of a variable name in a list of tuples
    indexOf :: String -> [(String, Type)] -> Int -> Maybe Int
    indexOf x list index = case list of 
      []                   -> Nothing -- Not found
      (id, ty) : ys        -> if x == id then Just index else indexOf x ys (index + 1)
