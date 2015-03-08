module Calculusbot.LanguageDef where

data CBExpr = Var String
          | Const Const
          | BinExpr BinOp CBExpr CBExpr
          | UnExpr UnOp CBExpr
          deriving (Eq)

data BinOp = Plus
           | Minus
           | Times
           | Divide
           | Power
           deriving (Eq)

data UnOp = Log
          | Sin
          | Cos
          | Tan
          | Sinh
          | Cosh
          | Tanh
          | Neg
          deriving (Eq)

data Const = E
           | Pi
           | IntLit Integer
           deriving (Eq)
