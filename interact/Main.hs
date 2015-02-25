module Main where

import qualified Calculusbot as CB

import Control.Monad
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

main = interact $ show . getExpr

getExpr :: String -> CB.Expr
getExpr input = case parse expParser "" input of
                    Left e -> error $ show e
                    Right r -> r


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

ops = [ [Prefix (reservedOp "-"      >> return (CB.Neg              ))           ]
      , [Infix (reservedOp "^"      >> return (CB.BinExpr CB.Power )) AssocRight]
      , [Infix (reservedOp "**"     >> return (CB.BinExpr CB.Power )) AssocRight]
      , [Infix (reservedOp "*"      >> return (CB.BinExpr CB.Times )) AssocLeft]
      , [Infix (reservedOp "/"      >> return (CB.BinExpr CB.Divide)) AssocLeft]
      , [Infix (reservedOp "+"      >> return (CB.BinExpr CB.Plus  )) AssocLeft]
      , [Infix (reservedOp "-"      >> return (CB.BinExpr CB.Minus )) AssocLeft]
      ]

terms = parens expParser
      <|> do { reserved "e"
             ; return (CB.Const CB.E)
             }
      <|> do { reserved "pi"
             ; return (CB.Const CB.Pi)
             }
      <|> do { reserved "log"
             ; e <- parens expParser
             ; return (CB.BinExpr CB.Log (CB.Const CB.E) e)
             }
      <|> do { reserved "logBase"
             ; char '('
             ; e1 <- expParser
             ; char ','
             ; e2 <- expParser
             ; char ')'
             ; return (CB.BinExpr CB.Log e1 e2)
             }
      <|> do { reserved "sinh"
             ; e <- parens expParser
             ; return (CB.UnExpr CB.Sinh e)
             }
      <|> do { reserved "cosh"
             ; e <- parens expParser
             ; return (CB.UnExpr CB.Cosh e)
             }
      <|> do { reserved "tanh"
             ; e <- parens expParser
             ; return (CB.UnExpr CB.Tanh e)
             }
      <|> do { reserved "sin"
             ; e <- parens expParser
             ; return (CB.UnExpr CB.Sin e)
             }
      <|> do { reserved "cos"
             ; e <- parens expParser
             ; return (CB.UnExpr CB.Cos e)
             }
      <|> do { reserved "tan"
             ; e <- parens expParser
             ; return (CB.UnExpr CB.Tan e)
             }
      <|> liftM CB.Var identifier
      <|> do { i <- integer
             ; return (CB.Const (CB.IntLit i))
             }
