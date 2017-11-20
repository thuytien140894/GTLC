module Parser (
  parseExpr
) where

  import Syntax
  import Lexer
  
  import Text.Parsec
  import Text.Parsec.String (Parser)

  -- -- if/then/else
  -- conditional :: Parser Expr
  -- conditional = do
  --   reserved "if"
  --   cond <- expr
  --   reservedOp "then"
  --   true <- expr
  --   reserved "else"
  --   false <- expr
  --   return (If cond true false)

  -- abstraction
  lambda :: Parser Term
  lambda = do 
    reserved "\\"
    recordVariable identifier 
    reserved "."
    body <- expr
    return (Lambda body)

  -- variable
  var :: Parser Term
  var = do
    variable <- identifier
    return (Var 0)

  -- Constants
  true, false :: Parser Term
  true  = reserved "true"  >> return Tr
  false = reserved "false" >> return Fl
  -- zero  = reservedOp "0"   >> return Zero

  -- application
  apply :: Term -> Term -> Term
  apply t1 t2 = case t1 of 
    Undefined    -> t2        -- t2 is the first term 
    _            -> App t1 t2

  -- recursively apply terms from the left
  applyFromLeft :: [Term] -> Term
  applyFromLeft = foldl apply Undefined

  -- apply two terms that are separated by a space
  app :: Parser Term
  app = do
    terms <- sepBy1 expr' whiteSpace 
    return (applyFromLeft terms)

  expr :: Parser Term
  expr = app 

  expr' :: Parser Term
  expr' = true
      <|> false
      <|> var
      <|> lambda
      <|> parens expr'

  -- remove the initial whitespace, line comments, and block comments 
  -- the parser only removes white spaces after the tokens
  removeWhiteSpace :: Parser Term
  removeWhiteSpace = whiteSpace >> expr

  parseExpr :: String -> Either ParseError Term
  parseExpr s = parse removeWhiteSpace "" s