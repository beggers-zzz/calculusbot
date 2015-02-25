module Main where

import qualified Calculusbot as CB
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

main = print getExpr

getExpr :: CB.Expr
getExpr = (CB.NumLit 7)
