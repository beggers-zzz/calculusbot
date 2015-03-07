module Calculusbot.Differentiate(
    differentiate
) where

import Calculusbot.LanguageDef

differentiate :: CBExpr -> CBExpr
differentiate = d

d (Const c) = (Const (IntLit 0))
d (Var s) = (Const (IntLit 1))
d (BinExpr op l r) = dBinOp op l r
d (UnExpr op e) = dUnOp op e

dBinOp :: BinOp -> CBExpr -> CBExpr -> CBExpr
dBinOp Plus l r = (BinExpr Plus (d l) (d r))
dBinOp Minus l r = (BinExpr Minus (d l) (d r))
dBinOp Times l r = productRule l r
dBinOp Divide top bot = quotientRule top bot
dBinOp Power b e = powerRule b e

dUnOp :: UnOp -> CBExpr -> CBExpr
dUnOp Sin e = (BinExpr Times (d e) (UnExpr Cos e))
dUnOp Cos e = (BinExpr Times (d e) (UnExpr Neg (UnExpr Sin e)))
dUnOp Tan e = (BinExpr Divide (d e) (BinExpr Power (UnExpr Cos e) (Const (IntLit 2))))
dUnOp Sinh e = (BinExpr Times (d e) (UnExpr Cosh e))
dUnOp Cosh e = (BinExpr Times (d e) (UnExpr Sinh e))
dUnOp Tanh e = (BinExpr Minus (Const (IntLit 1)) (BinExpr Power (UnExpr Tanh e) (Const (IntLit 2))))
dUnOp Neg e = (UnExpr Neg (d e))
dUnOp op e = (UnExpr op e)

productRule :: CBExpr -> CBExpr -> CBExpr
productRule l r = (BinExpr Plus (BinExpr Times (d l) r) (BinExpr Times l (d r)))

quotientRule :: CBExpr -> CBExpr -> CBExpr
quotientRule top bot = (BinExpr Divide
                            (BinExpr Minus
                                (BinExpr Times
                                    bot
                                    (d top)
                                )
                                (BinExpr Times
                                    top
                                    (d bot)
                                )
                            )
                            (BinExpr Power
                                bot
                                (Const (IntLit 2))   -- "magic number" my ass
                            )
                        )

-- http://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule
powerRule :: CBExpr -> CBExpr -> CBExpr
powerRule b e = (BinExpr Times
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
                            (d b)
                        )
                        (BinExpr Times
                            (BinExpr Times
                                (UnExpr Log b)
                                b
                            )
                            (d e)
                        )
                    )
                )
