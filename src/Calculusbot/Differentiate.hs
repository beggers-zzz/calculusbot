module Calculusbot.Differentiate(
    differentiate
) where

import Calculusbot.LanguageDef
import Calculusbot.Simplify

differentiate :: CBExpr -> String -> CBExpr
differentiate e s = simplify $ d e s

d :: CBExpr -> String -> CBExpr
d (Const c) s = (Const (IntLit 0))
d (Var x) s
    | s == x        = (Const (IntLit 1))
    | otherwise     = (Const (IntLit 0))
d (BinExpr op l r) s = dBinOp op l r s
d (UnExpr op e) s = dUnOp op e s

dBinOp :: BinOp -> CBExpr -> CBExpr -> String -> CBExpr
dBinOp Plus l r s = (BinExpr Plus (d l s) (d r s))
dBinOp Minus l r s = (BinExpr Minus (d l s) (d r s))
dBinOp Times l r s = productRule l r s
dBinOp Divide top bot s = quotientRule top bot s
dBinOp Power b e s = powerRule b e s

dUnOp :: UnOp -> CBExpr -> String -> CBExpr
dUnOp Sin e s = (BinExpr Times (d e s) (UnExpr Cos e))
dUnOp Cos e s = (BinExpr Times (d e s) (UnExpr Neg (UnExpr Sin e)))
dUnOp Tan e s = (BinExpr Divide (d e s) (BinExpr Power (UnExpr Cos e) (Const (IntLit 2))))
dUnOp Sinh e s = (BinExpr Times (d e s) (UnExpr Cosh e))
dUnOp Cosh e s = (BinExpr Times (d e s) (UnExpr Sinh e))
dUnOp Tanh e s = (BinExpr Times (BinExpr Minus (Const (IntLit 1)) (BinExpr Power (UnExpr Tanh e) (Const (IntLit 2)))) (d e s))  -- (d/ds e) * (1 - Tanh^2(e))
dUnOp Neg e s = (UnExpr Neg (d e s))
dUnOp Log e s = (BinExpr Divide (d e s) e)

productRule :: CBExpr -> CBExpr -> String -> CBExpr
productRule l r s = (BinExpr Plus (BinExpr Times (d l s) r) (BinExpr Times l (d r s)))

quotientRule :: CBExpr -> CBExpr -> String ->  CBExpr
quotientRule top bot s = (BinExpr Divide
                            (BinExpr Minus
                                (BinExpr Times
                                    bot
                                    (d top s)
                                )
                                (BinExpr Times
                                    top
                                    (d bot s)
                                )
                            )
                            (BinExpr Power
                                bot
                                (Const (IntLit 2))   -- "magic number" my ass
                            )
                        )

-- http://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule
powerRule :: CBExpr -> CBExpr -> String -> CBExpr
powerRule b e s = (BinExpr Times
                    (BinExpr Power
                        b
                        (BinExpr Minus
                            e
                            (Const (IntLit 1))
                        )
                    )
                    (BinExpr Plus
                        (BinExpr Times
                            e
                            (d b s)
                        )
                        (BinExpr Times
                            (BinExpr Times
                                (UnExpr Log b)
                                b
                            )
                            (d e s)
                        )
                    )
                )
