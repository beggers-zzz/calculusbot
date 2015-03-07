module Calculusbot.Unparse(
    unparseExpr
) where

import Calculusbot.LanguageDef
import Calculusbot.Simplify

unparseExpr :: CBExpr -> String
unparseExpr e = unparseExpr' (simplify e) ++ "\n"

unparseExpr' :: CBExpr -> String
unparseExpr' (Var x) = x
unparseExpr' (Const c) = unparsec c
unparseExpr' (BinExpr op l r) = "(" ++ (unparseExpr' l) ++ (unparsebin op) ++ (unparseExpr' r) ++ ")"
unparseExpr' (UnExpr op e) = (unparseun op) ++ "(" ++ (unparseExpr' e) ++ ")"

unparsec :: Const -> String
unparsec E = "e"
unparsec Pi = "pi"
unparsec (IntLit i) = show i

unparsebin :: BinOp -> String
unparsebin Plus = " + "
unparsebin Minus = " - "
unparsebin Times = " * "
unparsebin Divide = " / "
unparsebin Power = "^"

unparseun :: UnOp -> String
unparseun Log = "log"
unparseun Neg = "-"
unparseun Sin = "sin"
unparseun Cos = "cos"
unparseun Tan = "tan"
unparseun Sinh = "sinh"
unparseun Cosh = "cosh"
unparseun Tanh = "tanh"
