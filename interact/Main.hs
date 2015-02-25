module Main where

import qualified Calculusbot as CB

main = print getExpr

getExpr :: CB.Expr
getExpr = (CB.NumLit 7)
