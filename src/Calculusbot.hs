
module Calculusbot where

data Expr = Var Char
          | NumLit Integer
          | BinExpr Expr BinOp Expr
          deriving (Show)
data BinOp = Plus
           | Minus
           | Times
           | Divide
           deriving (Show)
