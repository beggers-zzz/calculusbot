module Calculusbot where

data Expr = Var String
          | Const Const
          | Neg Expr
          | BinExpr BinOp Expr Expr
          | UnExpr UnOp Expr
          deriving (Show)

data BinOp = Plus
           | Minus
           | Times
           | Divide
           | Power
           | Log
           deriving (Show)

data UnOp = Sin
          | Cos
          | Tan
          | Sinh
          | Cosh
          | Tanh
          deriving (Show)

data Const = E
           | Pi
           | IntLit Integer
           deriving (Show)
