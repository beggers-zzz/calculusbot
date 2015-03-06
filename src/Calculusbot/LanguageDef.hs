module Calculusbot.LanguageDef where

data CBExpr = Var String
          | Const Const
          | BinExpr BinOp CBExpr CBExpr
          | UnExpr UnOp CBExpr
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
          | Neg
          deriving (Show)

data Const = E
           | Pi
           | IntLit Integer
           deriving (Show)

