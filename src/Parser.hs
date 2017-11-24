module Parser (
  parseExpr
) where

  import Syntax
  import Lexer

  import Data.Maybe
  import Data.List
  import Text.Parsec
  import Text.Parsec.String (Parser)
  import qualified Text.Parsec.Expr as Ex
  import Data.Functor.Identity

  -- if statement
  conditional :: Parser Term
  conditional = do
    reserved "if"
    cond <- expr
    reserved "then"
    tr <- expr
    reserved "else"
    fl <- expr
    return $ If cond tr fl

  -- abstraction
  lambda :: Parser Term
  lambda = do 
    reserved "\\"
    arg <- identifier 
    reserved "."
    body <- expr
    let ctx = arg : getContext body
    return $ fixBinding (Lambda body ctx) ctx

  -- variable
  var :: Parser Term
  var = do
    id <- identifier
    return $ Var 0 id

  -- Constants
  true, false :: Parser Term
  true  = reserved "true"  >> return Tru
  false = reserved "false" >> return Fls
  zero  = reservedOp "0"   >> return Zero

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
    return $ applyFromLeft terms

  -- parse an application which consists a sequence of terms
  expr :: Parser Term
  expr = app 
  
  -- Prefix operators
  prefixTable :: Ex.OperatorTable String () Identity Term
  prefixTable = [
      [
        Ex.Prefix (reserved "succ"   >> return Succ)
      , Ex.Prefix (reserved "pred"   >> return Pred)
      , Ex.Prefix (reserved "iszero" >> return IsZero)
      ]
    ]

  -- parse an arithmetic expression such succ, pred, and iszero
  arith :: Parser Term
  arith = Ex.buildExpressionParser prefixTable nat

  -- restrict arithmetic expressions to only accept numeric values
  nat :: Parser Term
  nat = zero
      <|> parens arith

  -- parse individual terms
  expr' :: Parser Term
  expr' = true
      <|> false
      <|> zero
      <|> var
      <|> lambda
      <|> conditional
      <|> arith
      <|> parens expr -- parse 'application' inside parenthesis

  -- remove the initial whitespace, line comments, and block comments 
  -- the parser only removes white spaces after the tokens
  removeWhiteSpace :: Parser Term
  removeWhiteSpace = whiteSpace >> expr

  -- parse a string
  parseExpr :: String -> Either ParseError Term
  parseExpr = parse removeWhiteSpace "" 

  -- correct the bruijn index for each variable
  -- this function is called when parsing a Lambda term 
  fixBinding :: Term -> [String] -> Term
  fixBinding t globalCtx = case t of
    Lambda t1 localCtx   -> Lambda (fixBinding t1 globalCtx) localCtx
    App t1 t2            -> App (fixBinding t1 globalCtx) (fixBinding t2 globalCtx)
    Var x id             -> Var (getBruijnIndex id globalCtx) id
    _                    -> t

  -- get the bruijn index for a variable with a given identifier and its binding context
  getBruijnIndex :: String -> [String] -> Int
  getBruijnIndex id ctx = fromMaybe (-1) (elemIndex id (reverse ctx))
