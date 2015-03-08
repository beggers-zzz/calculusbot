module Calculusbot.Simplify where

import Calculusbot.LanguageDef

simplify :: CBExpr -> CBExpr
simplify (BinExpr Plus l r)
    | sl == (Const (IntLit 0))      = sr
    | sr == (Const (IntLit 0))      = sl
    | otherwise                     = (BinExpr Plus sl sr)
    where
        sr = simplify r
        sl = simplify l

simplify (BinExpr Minus l r)
    | sr == (Const (IntLit 0))      = sl
    | sl == (Const (IntLit 0))      = (UnExpr Neg sr)
    | otherwise                     = (BinExpr Minus sl sr)
    where
        sr = simplify r
        sl = simplify l

simplify e = e
