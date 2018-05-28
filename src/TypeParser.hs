module TypeParser where 

    import Lexer
    import ParseUtils (arrowFromRight)
    import Type

    import Text.Parsec
    import Text.Parsec.String (Parser)

    -- | Parse base types.
    boolean, nat, top, dynamic :: Parser Type
    boolean = reserved "Bool" >> return Boolean
    nat     = reserved "Nat"  >> return Nat
    top     = reserved "Top"  >> return Top
    dynamic = reserved "Dyn"  >> return Dyn
    
    -- | Parse reference types. 
    ref :: Parser Type 
    ref = do 
        reserved "Ref"
        ty <- types 
        return $ TRef ty

    -- | Parse a record type.
    recordTy :: Parser Type
    recordTy = braces recordTy'

    -- | Parse a record of many entries separated 
    -- by a comma.
    recordTy' :: Parser Type
    recordTy' = do
        list <- sepBy1 entryTy comma
        return $ TRec $ concat list

    -- | Parse one entry of a record.
    entryTy :: Parser [TEntry]
    entryTy = do
        field <- identifier 
        colon 
        ty <- types
        return [(field, ty)]

    -- | Parse one or more types separated by "->" 
    -- and apply them from right to left.
    types :: Parser Type
    types = do
        list <- sepBy1 types' arrowSep
        return $ arrowFromRight list

    -- | Parse a type.
    types' :: Parser Type
    types' = parens types 
         <|> boolean
         <|> nat
         <|> top
         <|> dynamic
         <|> ref
         <|> recordTy