module JSFuck.Parser exposing (parse)

import JSFuck.Expr exposing (Expr(..))
import Parser exposing (..)


parse : String -> Result (List Parser.DeadEnd) Expr
parse s =
    Parser.run (root |. end) s



{-
   Precendence
   expr+expr
   !expr, +expr
   expr[expr], expr(expr), expr()
   (expr), [expr], []
-}


parsersByPrecendence : List (Parser Expr -> Parser Expr -> Parser Expr)
parsersByPrecendence =
    [ \_ next ->
        next
            |> looper
                (\state ->
                    [ succeed (Add state)
                        |. symbol "+"
                        |= next
                    ]
                )
    , \current next ->
        oneOf
            [ unary "+" UnaryPlus current
            , unary "!" LogicalNot current
            , next
            ]
    , \_ next ->
        next
            |> looper
                (\state ->
                    [ withSubexpr (AttrAccessor state) ( "[", "]" ) -- attribute access
                    , withMaybeSubexpr (FunctionCall state) ( "(", ")" ) -- function call
                    ]
                )
    , \_ _ ->
        oneOf
            [ withSubexpr identity ( "(", ")" ) -- grouping
            , withMaybeSubexpr ArrayLiteral ( "[", "]" ) -- array literal
            ]
    ]


combineParsers : (Parser Expr -> Parser Expr -> Parser Expr) -> Parser Expr -> Parser Expr
combineParsers f next =
    let
        current _ =
            lazy (\_ -> f (current ()) next)
    in
    f (current ()) next


root : Parser Expr
root =
    List.foldr
        combineParsers
        (problem "Bug: Parser next level is not defined.")
        parsersByPrecendence


withSubexpr : (Expr -> Expr) -> ( String, String ) -> Parser Expr
withSubexpr f pair =
    succeed f
        |. symbol (Tuple.first pair)
        |= lazy (\_ -> root)
        |. symbol (Tuple.second pair)


withMaybeSubexpr : (Maybe Expr -> Expr) -> ( String, String ) -> Parser Expr
withMaybeSubexpr f pair =
    succeed f
        |. symbol (Tuple.first pair)
        |= oneOf
            [ succeed Nothing
                |. symbol (Tuple.second pair)
            , succeed Just
                |= lazy (\_ -> root)
                |. symbol (Tuple.second pair)
            ]


unary : String -> (Expr -> Expr) -> Parser Expr -> Parser Expr
unary s e current =
    succeed e
        |. symbol s
        |= current


loopHelper : (Expr -> List (Parser Expr)) -> Expr -> Parser (Step Expr Expr)
loopHelper subParsers state =
    List.map (\f -> map Loop f) (subParsers state)
        ++ [ succeed <| Done state ]
        |> oneOf


looper : (Expr -> List (Parser Expr)) -> Parser Expr -> Parser Expr
looper subParsers =
    andThen
        (\x -> loop x <| loopHelper subParsers)
