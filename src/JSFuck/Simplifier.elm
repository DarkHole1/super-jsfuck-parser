module JSFuck.Simplifier exposing (simplify, uneval)

import JSFuck.Expr exposing (Expr(..))


uneval : Expr -> Expr
uneval x =
    case x of
        FunctionCall (FunctionCall (AttrAccessor (AttrAccessor (ArrayLiteral Nothing) (StringLiteral "flat")) (StringLiteral "constructor")) (Just (StringLiteral y))) Nothing ->
            StringLiteral y

        FunctionCall (FunctionCall (FunctionCall (AttrAccessor (AttrAccessor (ArrayLiteral Nothing) (StringLiteral "flat")) (StringLiteral "constructor")) (Just (StringLiteral "return eval"))) Nothing) (Just (StringLiteral y)) ->
            StringLiteral y

        _ ->
            x


simplify : Expr -> Expr
simplify expr =
    let
        simplifier =
            logicalNot << attrAccessor << unaryPlus << binaryPlus << functionCall
    in
    exprMap simplifier expr


logicalNot : Expr -> Expr
logicalNot x =
    case x of
        LogicalNot (ArrayLiteral Nothing) ->
            BoolLiteral False

        LogicalNot (BoolLiteral y) ->
            BoolLiteral (not y)

        LogicalNot (NumberLiteral y) ->
            BoolLiteral (y == 0)

        _ ->
            x


constructorName : Expr -> Maybe String
constructorName expr =
    case expr of
        ArrayLiteral _ ->
            Just "Array"

        StringLiteral _ ->
            Just "String"

        BoolLiteral _ ->
            Just "Boolean"

        Function _ _ ->
            Just "Function"

        NumberLiteral _ ->
            Just "Number"

        Window ->
            Just "Window"

        _ ->
            Nothing


attrAccessor : Expr -> Expr
attrAccessor x =
    case x of
        AttrAccessor (ArrayLiteral Nothing) (ArrayLiteral Nothing) ->
            UndefinedLiteral

        AttrAccessor (StringLiteral y) (NumberLiteral z) ->
            String.slice z (z + 1) y
                |> StringLiteral

        -- TODO: More carefully convert accessor to functions
        AttrAccessor y (StringLiteral "constructor") ->
            constructorName y
                -- Constructors can safely ignore context
                |> Maybe.map (\name -> Function name Window)
                |> Maybe.withDefault x

        AttrAccessor (ArrayLiteral z) (StringLiteral y) ->
            Function y (ArrayLiteral z)

        AttrAccessor (NumberLiteral z) (StringLiteral y) ->
            Function y (NumberLiteral z)

        AttrAccessor (Function name _) (StringLiteral "name") ->
            StringLiteral name

        AttrAccessor (StringLiteral y) (StringLiteral z) ->
            String.toInt z
                |> Maybe.map (\n -> String.slice n (n + 1) y)
                |> Maybe.map StringLiteral
                |> Maybe.withDefault (Function z (StringLiteral y))

        _ ->
            x


unaryPlus : Expr -> Expr
unaryPlus x =
    case x of
        UnaryPlus (ArrayLiteral (Just (BoolLiteral False))) ->
            NaNLiteral

        UnaryPlus (ArrayLiteral Nothing) ->
            NumberLiteral 0

        UnaryPlus (BoolLiteral y) ->
            NumberLiteral
                (if y then
                    1

                 else
                    0
                )

        UnaryPlus (StringLiteral y) ->
            String.toInt y
                |> Maybe.map NumberLiteral
                |> Maybe.withDefault NaNLiteral

        _ ->
            x


isString : Expr -> Bool
isString expr =
    case expr of
        StringLiteral _ ->
            True

        _ ->
            False


jsConcat : Expr -> Expr -> Maybe Expr
jsConcat e1 e2 =
    case ( e1, e2 ) of
        ( StringLiteral x, StringLiteral y ) ->
            StringLiteral (x ++ y)
                |> Just

        _ ->
            Nothing


{-| <https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tonumber>
-}
toNumber : Expr -> Maybe Expr
toNumber expr =
    case expr of
        UndefinedLiteral ->
            Just NaNLiteral

        BoolLiteral True ->
            Just <| NumberLiteral 1

        BoolLiteral False ->
            Just <| NumberLiteral 0

        NumberLiteral _ ->
            Just expr

        ArrayLiteral _ ->
            toPrimitive expr
                |> Maybe.andThen toNumber

        Function _ _ ->
            toPrimitive expr
                |> Maybe.andThen toNumber

        Window ->
            toPrimitive expr
                |> Maybe.andThen toNumber

        _ ->
            Nothing


{-| <https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tonumeric>
-}
toNumeric : Expr -> Maybe Expr
toNumeric expr =
    expr
        |> toPrimitive
        |> Maybe.andThen toNumber


jsAdd : Expr -> Expr -> Maybe Expr
jsAdd e1 e2 =
    case ( e1, e2 ) of
        ( NumberLiteral x, NumberLiteral y ) ->
            Just <| NumberLiteral (x + y)

        _ ->
            Nothing


{-| <https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-applystringornumericbinaryoperator>
-}
jsBinaryPlus : Expr -> Expr -> Maybe Expr
jsBinaryPlus lval rval =
    let
        maybeLprim =
            toPrimitive (Debug.log "lval" lval)
                |> Debug.log "converted lval"

        maybeRprim =
            toPrimitive (Debug.log "rval" rval)
                |> Debug.log "converted rval"
    in
    Maybe.map2 Tuple.pair maybeLprim maybeRprim
        |> Maybe.andThen
            (\( lprim, rprim ) ->
                if isString lprim || isString rprim then
                    Maybe.map2 Tuple.pair (toString lprim) (toString rprim)
                        |> Maybe.andThen (\( lprim2, rprim2 ) -> jsConcat lprim2 rprim2)

                else
                    Maybe.map2 Tuple.pair (toNumeric lprim) (toNumeric rprim)
                        |> Maybe.andThen (\( lprim2, rprim2 ) -> jsAdd lprim2 rprim2)
            )


binaryPlus : Expr -> Expr
binaryPlus expr =
    case expr of
        Add x y ->
            jsBinaryPlus x y
                |> Maybe.withDefault expr
                |> Debug.log "Plus"

        _ ->
            expr


toBase : Int -> Int -> String
toBase base x =
    let
        current =
            modBy base x

        rest =
            x // base

        currentEncoded =
            String.slice current (current + 1) "0123456789abcdefghijklmnopqrstuvwxyz"
    in
    if rest == 0 then
        currentEncoded

    else
        toBase base rest ++ currentEncoded


functionCall : Expr -> Expr
functionCall x =
    case x of
        FunctionCall (Function "toString" (NumberLiteral y)) (Just (StringLiteral z)) ->
            String.toInt z
                |> Maybe.map (\n -> toBase n y)
                |> Maybe.map StringLiteral
                |> Maybe.withDefault x

        FunctionCall (FunctionCall (Function "Function" _) (Just (StringLiteral "return eval"))) Nothing ->
            Function "eval" Window

        FunctionCall (FunctionCall (Function "Function" _) (Just (StringLiteral y))) Nothing ->
            Raw y

        FunctionCall (Function "eval" Window) (Just (StringLiteral y)) ->
            Raw y

        FunctionCall (Function "fontcolor" (StringLiteral y)) Nothing ->
            "<font color=\"undefined\">"
                ++ y
                ++ "</font>"
                |> StringLiteral


        FunctionCall (Function "italics" (StringLiteral y)) Nothing ->
            "<i>"
                ++ y
                ++ "</i>"
                |> StringLiteral

        -- FunctionCall (Function "split" (StringLiteral y)) (Just (StringLiteral z)) ->
        --     Debug.todo "I need implement variable length Array :C"
        _ ->
            x


rawCode : Expr -> Expr
rawCode expr =
    case expr of
        Raw "return/false/" ->
            -- Regex
            Debug.todo "Add regex"
        _ ->
            expr

{-| Converts value to string, if it isn't a literal, return Nothing
-}
toString : Expr -> Maybe Expr
toString expr =
    case expr of
        StringLiteral _ ->
            Just expr

        ArrayLiteral Nothing ->
            Just <| StringLiteral ""

        ArrayLiteral (Just x) ->
            toString x

        NaNLiteral ->
            StringLiteral "NaN"
                |> Just

        NumberLiteral x ->
            String.fromInt x
                |> StringLiteral
                |> Just

        UndefinedLiteral ->
            StringLiteral "undefined"
                |> Just

        BoolLiteral True ->
            Just <| StringLiteral "true"

        BoolLiteral False ->
            Just <| StringLiteral "false"

        Function name _ ->
            -- Source code may vary, but is jsfuck this only what's possible
            "function "
                ++ name
                ++ "() { [native code] }"
                |> StringLiteral
                |> Just

        _ ->
            Nothing


isLiteral : Expr -> Bool
isLiteral expr =
    case expr of
        ArrayLiteral _ ->
            True

        StringLiteral _ ->
            True

        BoolLiteral _ ->
            True

        UndefinedLiteral ->
            True

        NaNLiteral ->
            True

        NumberLiteral _ ->
            True

        _ ->
            False


isObject : Expr -> Bool
isObject expr =
    case expr of
        ArrayLiteral _ ->
            True

        Function _ _ ->
            True

        _ ->
            False


{-| Try to convert value to primitive. If value isn't literal, returns `Nothing`

<https://tc39.es/ecma262/multipage/abstract-operations.html#sec-toprimitive>

-}
toPrimitive : Expr -> Maybe Expr
toPrimitive expr =
    if isObject expr then
        -- Actually, it can declare toValue, but in jsfuck that's impossible
        toString expr

    else if isLiteral expr then
        Just expr

    else
        Nothing


exprMap : (Expr -> Expr) -> Expr -> Expr
exprMap f =
    f << exprMapHelper f


exprMapHelper : (Expr -> Expr) -> Expr -> Expr
exprMapHelper f e =
    case e of
        ArrayLiteral Nothing ->
            e

        ArrayLiteral (Just x) ->
            exprMap f x
                |> Just
                |> ArrayLiteral

        AttrAccessor x y ->
            AttrAccessor (exprMap f x) (exprMap f y)

        UnaryPlus x ->
            exprMap f x
                |> UnaryPlus

        Add x y ->
            Add (exprMap f x) (exprMap f y)

        LogicalNot x ->
            exprMap f x
                |> LogicalNot

        FunctionCall x Nothing ->
            FunctionCall (exprMap f x) Nothing

        FunctionCall x (Just y) ->
            FunctionCall (exprMap f x) (Just (exprMap f y))

        BoolLiteral _ ->
            e

        UndefinedLiteral ->
            e

        NaNLiteral ->
            e

        NumberLiteral _ ->
            e

        StringLiteral _ ->
            e

        Function _ _ ->
            e

        Window ->
            e

        Raw _ ->
            e
