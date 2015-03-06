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
dBinOp Divide l r = quotientRule l r
dBinOp op l r = (BinExpr op l r)  -- temporary cop-out to get stuff mostly working

dUnOp :: UnOp -> CBExpr -> CBExpr
dUnOp op e = (UnExpr op e)

productRule :: CBExpr -> CBExpr -> CBExpr
productRule l r = (BinExpr Plus (BinExpr Times (d l) r) (BinExpr Times l (d r)))

quotientRule :: CBExpr -> CBExpr -> CBExpr
quotientRule l r = (BinExpr Divide
                        (BinExpr Minus
                            (BinExpr Times
                                l
                                (d r)
                            )
                            (BinExpr Times
                                r
                                (d l)
                            )
                        )
                        (BinExpr Power
                            l
                            (Const (IntLit 2))   -- "magic number" my ass
                        )
                    )


