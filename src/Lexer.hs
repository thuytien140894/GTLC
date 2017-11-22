module Lexer where
  
  import Syntax
  
  import Text.Parsec
  import Text.Parsec.String (Parser)
  import Text.Parsec.Language (emptyDef)
  
  import qualified Text.Parsec.Expr as Ex
  import qualified Text.Parsec.Token as Tok
  
  import Data.Functor.Identity
  
  -- language definition
  langDef :: Tok.LanguageDef ()
  langDef = Tok.LanguageDef
    { Tok.commentStart    = "" -- the language doesn't support block comments
    , Tok.commentEnd      = ""
    , Tok.commentLine     = "//"
    , Tok.nestedComments  = False
    , Tok.identStart      = letter 
    , Tok.identLetter     = alphaNum 
    , Tok.reservedNames   = [ "true"
                            , "false"
                            , "if"
                            , "then"
                            , "else"]
    , Tok.reservedOpNames = []
    , Tok.caseSensitive   = True
    }
  
  -- create a lexer
  lexer :: Tok.TokenParser ()
  lexer = Tok.makeTokenParser langDef
  
  -- parse p enclosed in parenthesis, returning the value of p
  parens :: Parser a -> Parser a
  parens = Tok.parens lexer
  
  -- parse a reserved name
  reserved :: String -> Parser ()
  reserved = Tok.reserved lexer
  
  -- parse an identifier
  identifier :: Parser String
  identifier = Tok.identifier lexer
  
  -- parse a white space
  whiteSpace :: Parser ()
  whiteSpace = Tok.whiteSpace lexer
  
  -- parse zero or more occurrences of p separated by semi
  -- returns a list of values returned by p
  semiSep :: Parser a -> Parser [a]
  semiSep = Tok.semiSep lexer
  
  -- parse an operator
  reservedOp :: String -> Parser ()
  reservedOp = Tok.reservedOp lexer
  
  prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
  prefixOp s f = Ex.Prefix (reservedOp s >> return f)
  
  -- Prefix operators
  table :: Ex.OperatorTable String () Identity Term
  table = [
      [
      --   prefixOp "succ" Succ
      -- , prefixOp "pred" Pred
      -- , prefixOp "iszero" IsZero
      ]
    ]