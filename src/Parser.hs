module Parser (
  parseExpr
  ) where

  import Syntax
  import Lexer
  import Types
  import ParseHelper 
  import TypeParser

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
    reservedOp "\\" >> whiteSpace
    arg <- identifier
    ty <- option Dyn $ try colon >> types -- if there is type specified, parse it; else return Dyn
    dot 
    body <- expr
    let t = fixBinding body arg 0
    let t' = updateVarType t arg ty
    let boundVars = arg : getBoundVar body
    let freeVars = getFreeVar body boundVars
    let t'' = fixFreeBinding t' freeVars boundVars
    return $ Lambda ty t'' boundVars

  -- parse a dereference
  dereference :: Parser Term 
  dereference = do 
    reservedOp "!" >> whiteSpace 
    t <- expr
    return $ Deref t

  -- parse a record
  record :: Parser Term 
  record = braces record'

  -- concaternate all the entries into a record
  record' :: Parser Term
  record' = do
    list <- sepBy1 entry comma
    return $ Rec $ concat list

  -- parse one entry of a record
  entry :: Parser [Entry]
  entry = do
    field <- identifier 
    reservedOp "=" >> whiteSpace
    value <- expr
    return [(field, value)]

  -- parse projection
  projection :: Parser Term
  projection = do
    t <- record
    dot
    label <- identifier
    return $ Proj t label

  -- variable
  var :: Parser Term
  var = do
    id <- identifier
    notProjection
    return $ Var (-1) TUnit id -- the variable is first parsed as free

  -- Constants
  true, false, zero :: Parser Term
  true  = reserved "true" >> notProjection >> return Tru
  false = reserved "false" >> notProjection >> return Fls
  zero  = reserved "0" >> notProjection >> return Zero

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
        Ex.Prefix $ reserved "succ"   >> return Succ
      , Ex.Prefix $ reserved "pred"   >> return Pred
      , Ex.Prefix $ reserved "iszero" >> return IsZero
      , Ex.Prefix $ reserved "ref"    >> return Ref
      , Ex.Infix (reservedOp ":=" >> return Assign) Ex.AssocLeft
      ]
    ]

  -- parse an arithmetic expression such as succ, pred, and iszero
  expr' :: Parser Term
  expr' = Ex.buildExpressionParser prefixTable expr''

  -- parse term enclosed in parenthesis
  parenExpr :: Parser Term
  parenExpr = do 
    t <- parens expr
    notProjection
    return t
    
  -- parse individual terms
  expr'' :: Parser Term
  expr'' = parenExpr
      <|> true
      <|> false
      <|> zero
      <|> var
      <|> lambda
      <|> conditional
      <|> try projection -- "look ahead" and see if an expression is a projection, if not, then move on
      <|> record
      <|> dereference

  -- remove the initial whitespace, line comments, and block comments 
  -- the parser only removes white spaces after the tokens
  removeWhiteSpace :: Parser Term
  removeWhiteSpace = whiteSpace >> expr 

  -- make sure a non-projection term should not be followed by '.'
  notProjection :: Parser ()
  notProjection = notFollowedBy dot

  -- parse a string
  parseExpr :: String -> Either ParseError Term
  parseExpr = parse removeWhiteSpace "" 