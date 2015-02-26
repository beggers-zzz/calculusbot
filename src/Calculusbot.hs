module Calculusbot where

import qualified Calculusbot.Parse as P
import Calculusbot.LanguageDef

parseExpr :: String -> Expr
parseExpr = P.parseExpr
