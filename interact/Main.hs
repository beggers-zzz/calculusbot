module Main where

import qualified Calculusbot as CB

import Control.Monad
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

main = print getExpr

getExpr :: CB.Expr
getExpr = (CB.Var "7")


languageDef = 
    emptyDef { Token.identStart         = letter
             , Token.identLetter        = letter
             , Token.reservedOpNames    = ["+", "-", "*", "-", "^", "**"]
             , Token.reservedNames      = ["sin", "cos", "tan", "sinh", "cosh", "tanh", "log", "logBase", "e", "pi"]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier   lexer
reserved = Token.reserved       lexer
reservedOp = Token.reservedOp   lexer
parens = Token.parens           lexer
integer = Token.integer         lexer

expParser = buildExpressionParser ops terms

ops = [ [Infix (reservedOp "^"      >> return (CB.BinExpr CB.Power )) AssocRight]
      , [Infix (reservedOp "**"     >> return (CB.BinExpr CB.Power )) AssocRight]
      , [Infix (reservedOp "*"      >> return (CB.BinExpr CB.Times )) AssocLeft]
      , [Infix (reservedOp "/"      >> return (CB.BinExpr CB.Divide)) AssocLeft]
      , [Infix (reservedOp "+"      >> return (CB.BinExpr CB.Plus  )) AssocLeft]
      , [Infix (reservedOp "-"      >> return (CB.BinExpr CB.Minus )) AssocLeft]
      ]

terms = parens expParser
      <|> liftM CB.Var identifier
      <|> do { reserved "sinh"
             ; char '('
             ; e <- expParser
             ; char ')'
             ; return (CB.UnExpr CB.Sinh e)
             }
      <|> do { reserved "cosh"
             ; char '('
             ; e <- expParser
             ; char ')'
             ; return (CB.UnExpr CB.Cosh e)
             }
      <|> do { reserved "tanh"
             ; char '('
             ; e <- expParser
             ; char ')'
             ; return (CB.UnExpr CB.Tanh e)
             }
      <|> do { reserved "sin"
             ; char '('
             ; e <- expParser
             ; char ')'
             ; return (CB.UnExpr CB.Sin e)
             }
      <|> do { reserved "cos"
             ; char '('
             ; e <- expParser
             ; char ')'
             ; return (CB.UnExpr CB.Cos e)
             }
      <|> do { reserved "tan"
             ; char '('
             ; e <- expParser
             ; char ')'
             ; return (CB.UnExpr CB.Tan e)
             }
