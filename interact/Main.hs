module Main where

import Calculusbot.Parse

main = interact $ show . parseExpr
