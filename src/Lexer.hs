module Lexer where
  
  import Syntax
  
  import Text.Parsec
  import Text.Parsec.String (Parser)
  import Text.Parsec.Language (emptyDef)
  import qualified Text.Parsec.Token as Tok
  
  -- language definition
  langDef :: Tok.LanguageDef ()
  langDef = Tok.LanguageDef
    { Tok.commentStart    = "" -- the language doesn't support block comments
    , Tok.commentEnd      = ""
    , Tok.commentLine     = "//"
    , Tok.nestedComments  = False
    , Tok.identStart      = letter 
    , Tok.identLetter     = alphaNum 
    , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames   = [ "true"
                            , "false"
                            , "if"
                            , "then"
                            , "else"
                            , "succ"
                            , "pred"
                            , "iszero"
                            , "zero"
                            , "Bool"
                            , "Nat" ]
    , Tok.reservedOpNames = [ "succ"
                            , "pred"
                            , "iszero" ]
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
  
  -- parse an arrow
  arrowSep :: Parser String
  arrowSep = string "->"

  -- parse a white space
  whiteSpace :: Parser ()
  whiteSpace = Tok.whiteSpace lexer
  
  -- parse an operator
  reservedOp :: String -> Parser ()
  reservedOp = Tok.reservedOp lexer