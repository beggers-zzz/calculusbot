module Main where

import Calculusbot.Parse
import Calculusbot.Unparse
import Calculusbot.Differentiate

main :: IO ()
main = do
    s <- getLine
    let e = parseExpr s
    let d = differentiate e "x"
    putStrLn $ unparseExpr d
