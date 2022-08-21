module JSFuck.Expr exposing (Expr(..), arguments, niceName, precedence, toSource)

{-| -}


{-| jsfuck expression, can be one of:

Basic:

  - `[]`
  - `[expr]`
  - `expr[expr]`
  - `+expr`
  - `expr+expr`
  - `!expr`
  - `expr()`
  - `expr(expr)`
  - `(expr)`

Extended:

  - true/false
  - undefined
  - NaN
  - integer number
  - "..."
  - function

Basic is for parsing and simplifing, extended is for simplifing only

-}
type
    Expr
    -- Basic
    = ArrayLiteral (Maybe Expr)
    | AttrAccessor Expr Expr
    | UnaryPlus Expr
    | Add Expr Expr
    | LogicalNot Expr
    | FunctionCall Expr (Maybe Expr)
      -- Extended
    | BoolLiteral Bool
    | UndefinedLiteral
    | NaNLiteral
    | NumberLiteral Int
    | StringLiteral String
    | Function String Expr
    | Window
    | Raw String


{-| Provides a human-readable string to describe expression.
Can be useful if you want to visualize expression.

    niceName (ArrayLiteral Nothing) == "Literal: Empty array"

    niceName (ArrayLiteral (Just (ArrayLiteral Nothing))) == "Literal: Array"

-}
niceName : Expr -> String
niceName expr =
    case expr of
        -- Basic
        ArrayLiteral Nothing ->
            "Literal: Empty array"

        ArrayLiteral (Just _) ->
            "Literal: Array"

        AttrAccessor _ _ ->
            "Attribute access"

        UnaryPlus _ ->
            "Unary plus"

        Add _ _ ->
            "Binary plus"

        LogicalNot _ ->
            "Logical not"

        FunctionCall _ Nothing ->
            "Empty function call"

        FunctionCall _ (Just _) ->
            "Function call"

        -- Extended
        BoolLiteral True ->
            "Literal: true"

        BoolLiteral False ->
            "Literal: false"

        UndefinedLiteral ->
            "Literal: undefined"

        NaNLiteral ->
            "Literal: NaN"

        NumberLiteral x ->
            "Literal: " ++ String.fromInt x

        StringLiteral x ->
            "Literal: \"" ++ x ++ "\""

        Function name _ ->
            "Pseudo-literal: function " ++ name
        
        Window ->
            "Pseudo-literal: window"

        Raw code ->
            "Raw code: " ++ code


{-| Returns precendence of expression
-}
precedence : Expr -> Int
precedence expr =
    case expr of
        Add _ _ ->
            1

        UnaryPlus _ ->
            2

        LogicalNot _ ->
            2

        AttrAccessor _ _ ->
            3

        FunctionCall _ _ ->
            3

        -- In fact, function is AttrAccessor, so they both have same precedence
        Function _ _ ->
            3

        ArrayLiteral _ ->
            10

        BoolLiteral _ ->
            10

        UndefinedLiteral ->
            10

        NaNLiteral ->
            10

        NumberLiteral _ ->
            10

        StringLiteral _ ->
            10
        
        Window ->
            10

        -- Raw code has lowest precendence, cause it always need braces
        Raw _ ->
            0


{-| Helper function, works like `toSource` but adds parethesis to keep
correct order of execution.

    toSourceWithPrecedence (LogicalNot _) (LogicalNot _) == "!..."

    toSourceWithPrecedence (FunctionCall _ _) (LogicalNot _) == "(!...)"

-}
toSourceWithPrecedence : Expr -> Expr -> String
toSourceWithPrecedence e1 e2 =
    let
        source =
            toSource e2
    in
    if precedence e1 > precedence e2 then
        "(" ++ source ++ ")"

    else
        source


escapeString : String -> String
escapeString =
    String.replace "\"" "\\\""

{-| Converts expression back to source. Useful for debugging.

    toSource (parse "+[[]()]") == "+[[]()]"

-}
toSource : Expr -> String
toSource expr =
    case expr of
        ArrayLiteral Nothing ->
            "[]"

        ArrayLiteral (Just x) ->
            "[" ++ toSource x ++ "]"

        AttrAccessor x y ->
            toSourceWithPrecedence expr x ++ "[" ++ toSource y ++ "]"

        UnaryPlus x ->
            "+" ++ toSourceWithPrecedence expr x

        Add x y ->
            toSourceWithPrecedence expr x ++ "+" ++ toSourceWithPrecedence expr y

        LogicalNot x ->
            "!" ++ toSourceWithPrecedence expr x

        FunctionCall x Nothing ->
            toSourceWithPrecedence expr x ++ "()"

        FunctionCall x (Just y) ->
            toSourceWithPrecedence expr x ++ "(" ++ toSource y ++ ")"

        BoolLiteral True ->
            "true"

        BoolLiteral False ->
            "false"

        UndefinedLiteral ->
            "undefined"

        NaNLiteral ->
            "NaN"

        NumberLiteral x ->
            String.fromInt x

        StringLiteral x ->
            "\"" ++ escapeString x ++ "\""

        Function x y ->
            toSourceWithPrecedence expr y ++ "[\"" ++ x ++ "\"]"
        
        Window ->
            "window"
        
        Raw x ->
            x


{-| Convert expression arguments to list of expressions. Note: literal arguments doesn't count

    arguments (FunctionCall (ArrayLiteral Nothing) (LogicalNot _)) == [ ArrayLiteral Nothing, LogicalNot _ ]

    arguments (ArrayLiteral Nothing) == []

    arguments (BoolLiteral True) == []

-}
arguments : Expr -> List Expr
arguments e =
    case e of
        ArrayLiteral Nothing ->
            []

        ArrayLiteral (Just x) ->
            [ x ]

        AttrAccessor x y ->
            [ x, y ]

        UnaryPlus x ->
            [ x ]

        Add x y ->
            [ x, y ]

        LogicalNot x ->
            [ x ]

        FunctionCall x Nothing ->
            [ x ]

        FunctionCall x (Just y) ->
            [ x, y ]

        BoolLiteral _ ->
            []

        UndefinedLiteral ->
            []

        NaNLiteral ->
            []

        NumberLiteral _ ->
            []

        StringLiteral _ ->
            []

        Function _ x ->
            [ x ]

        Window ->
            []

        Raw _ ->
            []
