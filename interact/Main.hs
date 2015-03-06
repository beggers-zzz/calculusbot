module Main where

import Calculusbot.Parse
import Calculusbot.Unparse

main = interact $ unparseExpr . parseExpr
