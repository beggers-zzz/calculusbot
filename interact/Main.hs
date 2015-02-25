module Main where

import qualified Calculusbot as CB

import Control.Monad
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

main = print getExpr

getExpr :: CB.Expr
getExpr = (CB.NumLit 7)



languageDef = 
    emptyDef {  Token.reservedOpNames = ["+", "-", "*", "-"]
             ,  Token.identStart = letter
             ,  Token.reservedNames = ["sin", "cos", "tan", "sinh", "cosh", "tanh", "log", "e"]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier   lexer
reserved = Token.reserved       lexer
reservedOp = Token.reservedOp   lexer
parens = Token.parens           lexer
integer = Token.integer         lexer

expParser = buildExpressionParser ops terms

ops = [ [Infix (reservedOp "*"  >> return (CB.BinExpr CB.Times )) AssocLeft]
      , [Infix (reservedOp "/"  >> return (CB.BinExpr CB.Divide)) AssocLeft]
      , [Infix (reservedOp "+"  >> return (CB.BinExpr CB.Plus  )) AssocLeft]
      , [Infix (reservedOp "-"  >> return (CB.BinExpr CB.Minus )) AssocLeft]
      ]

terms = parens expParser
      <|> liftM CB.Var identifier
      <|> liftM CB.NumLit integer
