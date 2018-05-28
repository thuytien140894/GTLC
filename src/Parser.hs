module Parser 
    ( parseExpr
    ) where

    import Lexer
    import ParseUtils 
    import Syntax
    import Type
    import TypeParser

    import Data.Functor.Identity
    import Text.Parsec
    import Text.Parsec.String (Parser)

    import qualified Text.Parsec.Expr as Ex
    
    -- | Make sure a non-projection term should not be followed by '.'.
    notProjection :: Parser ()
    notProjection = notFollowedBy dot

    -- | Parse a conditional statement.
    conditional :: Parser Term
    conditional = do
        reserved "if"
        cond <- expr
        reserved "then"
        tr <- expr
        reserved "else"
        fl <- expr
        return $ If cond tr fl

    -- | Parse an abstraction.
    lambda :: Parser Term
    lambda = do
        reservedOp "\\" 
        arg <- identifier
        -- | If there is type specified, parse it; else return Dyn.
        ty <- option Dyn $ try colon >> types  
        dot 
        body <- expr
        let t = fixBinding body arg 0
        let t' = updateVarType t arg ty
        let boundVars = arg : getBoundVar body
        let freeVars = getFreeVar body boundVars
        let t'' = fixFreeBinding t' freeVars boundVars
        return $ Lambda ty t'' boundVars

    -- | Parse a dereference.
    dereference :: Parser Term 
    dereference = do 
        reservedOp "!"
        t <- expr
        return $ Deref t

    -- | Parse a record enclosed in braces.
    record :: Parser Term 
    record = braces record'

    -- | Parse one or more record entries separated 
    -- by a comma, and concaternate them into a record.
    record' :: Parser Term
    record' = do
        list <- sepBy1 entry comma
        return $ Rec $ concat list

    -- | Parse one entry of a record.
    entry :: Parser [Entry]
    entry = do
        field <- identifier 
        reservedOp "="
        value <- expr
        return [(field, value)]

    -- | Parse a projection.
    projection :: Parser Term
    projection = do
        t <- record
        dot
        label <- identifier
        return $ Proj t label

    -- | Parse a variable.
    var :: Parser Term
    var = do
        id <- identifier
        notProjection
        return $ Var (-1) TUnit id  -- The variable is first parsed as free

    -- | Parse constants.
    true, false, zero :: Parser Term
    true  = reserved "true" >> notProjection >> return Tru
    false = reserved "false" >> notProjection >> return Fls
    zero  = reserved "0" >> notProjection >> return Zero

    -- | Parse one or more operations separated 
    -- by a space and apply them from left to right.
    app :: Parser Term
    app = do
        terms <- sepBy1 expr' whiteSpace 
        return $ applyFromLeft terms

    -- | At the top level, parse an expression as 
    -- an application first.
    expr :: Parser Term
    expr = app 

    -- | Parse unary prefix and binary infix operators.
    operatorTable :: Ex.OperatorTable String () Identity Term
    operatorTable = 
        [ [ Ex.Prefix $ reserved "succ"   >> return Succ
          , Ex.Prefix $ reserved "pred"   >> return Pred
          , Ex.Prefix $ reserved "iszero" >> return IsZero
          , Ex.Prefix $ reserved "ref"    >> return Ref
          , Ex.Infix (reservedOp ":=" >> return Assign) Ex.AssocLeft
          ]
        ]

    -- | Parse an operation.
    expr' :: Parser Term
    expr' = Ex.buildExpressionParser operatorTable expr''

    -- | Parse a term enclosed in parentheses.
    parenExpr :: Parser Term
    parenExpr = do 
        t <- parens expr
        notProjection
        return t
        
    -- | Parse an atomic expression.
    expr'' :: Parser Term
    expr'' = parenExpr
          <|> true
          <|> false
          <|> zero
          <|> var
          <|> lambda
          <|> conditional
          <|> try projection  -- "look ahead" for a projection
          <|> record
          <|> dereference

    -- | Parse an input program into its AST.
    parseExpr :: String -> Either ParseError Term
    parseExpr = parse (whiteSpace >> expr) "" 