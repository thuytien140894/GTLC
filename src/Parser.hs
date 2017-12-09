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
      lamb >> whiteSpace
      arg <- identifier 
      colon >> whiteSpace
      ty <- types
      dot >> whiteSpace
      body <- expr
      let boundVars = arg : getBoundVar body
      let freeVars = getFreeVar body boundVars 
      let t = fixBinding (Lambda ty body boundVars) boundVars freeVars
      return $ updateVarType t arg ty

    record :: Parser Term 
    record = braces record'

    -- parse a record of many entries
    record' :: Parser Term
    record' = do
      list <- sepBy1 entry comma
      return $ Rec (concat list)

    -- parse one entry of a record
    entry :: Parser [Entry]
    entry = do
      field <- identifier 
      equal >> whiteSpace -- parse any spaces after the equal sign
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
      return $ Var (-1) TUnit id -- the variable is first parsed as free

    -- Constants
    true, false :: Parser Term
    true  = reserved "true" >> return Tru
    false = reserved "false" >> return Fls
    zero  = reserved "0" >> return Zero

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
    arith = Ex.buildExpressionParser prefixTable num

    -- restrict arithmetic expressions to only accept numeric values
    num :: Parser Term
    num = zero
        <|> parens arith

    -- parse individual terms
    expr' :: Parser Term
    expr' = parens expr -- parse 'application' inside parenthesis
        <|> true
        <|> false
        <|> zero
        <|> var
        <|> lambda
        <|> conditional
        <|> arith
        <|> try projection -- "look ahead" and see if an expression is a projection, if not, then move on
        <|> record

    -- remove the initial whitespace, line comments, and block comments 
    -- the parser only removes white spaces after the tokens
    removeWhiteSpace :: Parser Term
    removeWhiteSpace = whiteSpace >> expr 

    -- parse a string
    parseExpr :: String -> Either ParseError Term
    parseExpr = parse removeWhiteSpace "" 