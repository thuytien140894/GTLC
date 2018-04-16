module TypeParser where 

  import Lexer
  import Types
  import ParseHelper (arrowFromRight)

  import Text.Parsec
  import Text.Parsec.String (Parser)

  -- base types
  boolean, nat, top, dynamic :: Parser Type
  boolean = reserved "Bool" >> return Bool
  nat = reserved "Nat" >> return Nat
  top = reserved "Top" >> return Top
  dynamic = reserved "Dyn" >> return Dyn
  
  -- reference types 
  ref :: Parser Type 
  ref = do 
    reserved "Ref"
    ty <- types 
    return $ TRef ty

  -- Record type
  recordTy :: Parser Type
  recordTy = braces recordTy'

  -- parse a record of many entries
  recordTy' :: Parser Type
  recordTy' = do
      list <- sepBy1 entryTy comma
      return $ TRec $ concat list

  -- parse one entry of a record
  entryTy :: Parser [TEntry]
  entryTy = do
      field <- identifier 
      colon >> whiteSpace -- parse any spaces after the equal sign
      ty <- types
      return [(field, ty)]

  -- parse a function type which consists of a sequence of types separated by "->"
  types :: Parser Type
  types = do
      list <- sepBy1 types' arrowSep
      return $ arrowFromRight list

  -- parse types
  types' :: Parser Type
  types' = parens types 
      <|> boolean
      <|> nat
      <|> top
      <|> dynamic
      <|> ref
      <|> recordTy