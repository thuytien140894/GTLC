module Parser (
  parseExpr
) where

  import Syntax
  import Lexer
  import Types

  import Data.Maybe
  import Data.List
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
    let boundVars = arg : getBindingContext body
    let freeVars = findFreeVar body boundVars 
    return $ fixBinding (Lambda ty body boundVars) boundVars freeVars

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
  fixBinding :: Term -> [String] -> [String] -> Term
  fixBinding t boundVars freeVars = case t of
    Lambda ty t1 localBound -> Lambda ty (fixBinding t1 boundVars freeVars) localBound
    App t1 t2             -> App (fixBinding t1 boundVars freeVars) (fixBinding t2 boundVars freeVars)
    Var _ id              -> Var (getBruijnIndex id boundVars freeVars) id
    _                     -> t

  -- get the bruijn index for a variable with a given identifier and its binding context
  getBruijnIndex :: String -> [String] -> [String] -> Int
  getBruijnIndex id boundVars freeVars 
    | isNothing boundIndex = fromJust freeIndex + length boundVars
    | otherwise            = fromJust boundIndex
    where boundIndex = elemIndex id (reverse boundVars)
          freeIndex = elemIndex id (reverse freeVars)

  -- find free variables
  findFreeVar :: Term -> [String] -> [String]
  findFreeVar t boundVars = case t of
    Lambda _ t1 _        -> findFreeVar t1 boundVars
    App t1 t2            -> findFreeVar t1 boundVars ++ findFreeVar t2 boundVars
    Var _ id             -> if id `isElem` boundVars then [] else [id]
    _                    -> []
      
  -- check if an element is in a list
  isElem :: String -> [String] -> Bool
  isElem x list = case list of 
    []             -> False
    y : ys         -> if x == y then True else isElem x ys
