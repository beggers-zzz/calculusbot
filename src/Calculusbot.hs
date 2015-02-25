
module Calculusbot where

data Expr = Var String
          | NumLit Integer
          | BinExpr BinOp Expr Expr
          deriving (Show)
data BinOp = Plus
           | Minus
           | Times
           | Divide
           deriving (Show)
