module Parser (
  parseExpr
  ) where

    import Syntax
    import Lexer
    import Types
    import ParseHelper 

    import Text.Parsec
    import Text.Parsec.String (Parser)
    import qualified Text.Parsec.Expr as Ex
    import Data.Functor.Identity

    -- Base types
    boolean, nat :: Parser Type
    boolean = reserved "Bool" >> return Bool
    nat = reserved "Nat" >> return Nat

    -- Function type
    function :: Parser Type
    function = do
      inType <- types
      reserved "->"
      outType <- types
      return $ Arr inType outType

    -- "arrow" two types
    arrow :: Type -> Type -> Type
    arrow t1 t2 = case t1 of 
      Void         -> t2        -- t2 is the first term 
      _            -> Arr t1 t2

    -- recursively "arrow" types from the left
    arrowFromLeft :: [Type] -> Type
    arrowFromLeft = foldl arrow Void

    -- parse a function type which consists of a sequence of types separated by "->"
    types :: Parser Type
    types = do
      list <- sepBy1 types' arrowSep
      return $ arrowFromLeft list

    -- parse types
    types' :: Parser Type
    types' = boolean 
          <|> nat
          <|> parens types

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
      reserved ":"
      ty <- types
      reserved "."
      body <- expr
      let boundVars = (arg, ty) : getBoundVar body
      let freeVars = getFreeVar body boundVars 
      let t = updateTypingEnv (Lambda ty body boundVars) boundVars
      return $ fixBinding t boundVars freeVars

    -- variable
    var :: Parser Term
    var = do
      id <- identifier
      return $ Var (-1) id

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

    -- remove the initial whitespace, line comments, and block comments 
    -- the parser only removes white spaces after the tokens
    removeWhiteSpace :: Parser Term
    removeWhiteSpace = whiteSpace >> expr

    -- parse a string
    parseExpr :: String -> Either ParseError Term
    parseExpr = parse removeWhiteSpace "" 