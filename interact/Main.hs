module Main where

import Calculusbot.Parse
import Calculusbot.Unparse
import Calculusbot.Differentiate

main = interact $ unparseExpr . differentiate . parseExpr
