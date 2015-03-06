module Calculusbot.LanguageDef where

data CBExpr = Var String
          | Const Const
          | BinExpr BinOp CBExpr CBExpr
          | UnExpr UnOp CBExpr

data BinOp = Plus
           | Minus
           | Times
           | Divide
           | Power

data UnOp = Log
          | Sin
          | Cos
          | Tan
          | Sinh
          | Cosh
          | Tanh
          | Neg

data Const = E
           | Pi
           | IntLit Integer

