module Calculusbot.Simplify where

import Calculusbot.LanguageDef

simplify :: CBExpr -> CBExpr
simplify (BinExpr Plus (Const (IntLit x)) (Const (IntLit y))) = (Const (IntLit (x + y)))
simplify (BinExpr Plus l r)
    | sr == (Const (IntLit 0))      = sl
    | sl == (Const (IntLit 0))      = sr
    | otherwise                     = (BinExpr Plus sl sr)
    where
        sr = simplify r
        sl = simplify l

simplify (BinExpr Minus (Const (IntLit x)) (Const (IntLit y))) = (Const (IntLit (x - y)))
simplify (BinExpr Minus l r)
    | sr == (Const (IntLit 0))      = sl
    | sl == (Const (IntLit 0))      = (UnExpr Neg sr)
    | otherwise                     = (BinExpr Minus sl sr)
    where
        sr = simplify r
        sl = simplify l

simplify (BinExpr Times (Const (IntLit x)) (Const (IntLit y))) = (Const (IntLit (x * y)))
simplify (BinExpr Times l r)
    | sr == (Const (IntLit 0))      = (Const (IntLit 0))
    | sl == (Const (IntLit 0))      = (Const (IntLit 0))
    | sr == (Const (IntLit 1))      = sl
    | sl == (Const (IntLit 1))      = sr
    | otherwise                     = (BinExpr Times sl sr)
    where
        sr = simplify r
        sl = simplify l

-- simplify (BinExpr Divide (Const (IntLit x)) (Const (IntLit y)) = (Const (IntLit (x / y)))
simplify (BinExpr Divide l r)
    | sr == (Const (IntLit 0))      = error "Divide by 0 error"
    | sl == (Const (IntLit 0))      = (Const (IntLit 0))
    | sr == (Const (IntLit 1))      = sl
    | otherwise                     = (BinExpr Divide sl sr)
    where
        sr = simplify r
        sl = simplify l

simplify (BinExpr Power b e)
    | se == (Const (IntLit 0))      = (Const (IntLit 1))
    | sb == (Const (IntLit 0))      = (Const (IntLit 0))
    | sb == (Const (IntLit 1))      = sb
    | se == (Const (IntLit 1))      = sb
    | otherwise                     = (BinExpr Power sb se)
    where
        sb = simplify b
        se = simplify e

simplify (UnExpr Log (Const E)) = (Const (IntLit 1))

simplify e = e
