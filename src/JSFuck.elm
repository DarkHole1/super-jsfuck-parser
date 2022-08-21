module JSFuck exposing (parse, simplify)

import JSFuck.Parser as P
import JSFuck.Simplifier as S
import Parser exposing (DeadEnd)
import JSFuck.Expr exposing (Expr)

parse : String -> Result (List DeadEnd) Expr
parse = P.parse

simplify : Expr -> Expr
simplify = S.simplify

uneval : Expr -> Expr
uneval = S.uneval