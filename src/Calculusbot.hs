module Calculusbot where

data Expr = Var String
          | Const Const
          | BinExpr BinOp Expr Expr
          | Sin Expr
          | Cos Expr
          | Tan Expr
          | Sinh Expr
          | Cosh Expr
          | Tanh Expr
          | Log Expr Expr
          deriving (Show)

data BinOp = Plus
           | Minus
           | Times
           | Divide
           | Power
           deriving (Show)

data Const = E
           | Pi
           | IntLit Integer
           deriving (Show)
