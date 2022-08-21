module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import JSFuck
import JSFuck.Expr as Expr exposing (Expr)
import Parser



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }



-- MODEL


type Tab
    = ParserOutput
    | ParsedTree
    | ParsedCode
    | SimplifiedTree
    | SimplifiedCode


type alias Model =
    { content : String
    , tab : Tab
    , parsed : Result (List Parser.DeadEnd) Expr
    , simplified : Maybe Expr
    }


init : Model
init =
    { content = ""
    , tab = SimplifiedCode
    , parsed = Result.Err []
    , simplified = Nothing
    }



-- UPDATE


type Msg
    = Change String
    | Select Tab


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change x ->
            let
                parsed =
                    JSFuck.parse x
            in
            { model
                | content = x
                , parsed = parsed
                , simplified =
                    Result.toMaybe parsed
                        |> Maybe.map JSFuck.simplify
            }

        Select tab ->
            { model | tab = tab }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "text-center" ]
            [ h1 [] [ text "Super jsfuck parser" ]
            ]
        , div [ class "main-input" ]
            [ input [ placeholder "Write jsfuck code", value model.content, onInput Change ] []
            ]
        , tabButtons model
            |> div [ class "tabs" ]
        , div [ class "main-content" ]
            [ parser model
            ]
        ]


tabButtons : Model -> List (Html Msg)
tabButtons model =
    [ button
        [ onClick <| Select ParserOutput
        , classList
            [ ( "active", model.tab == ParserOutput )
            ]
        ]
        [ text "Parser output" ]
    , button
        [ onClick <| Select ParsedTree
        , classList
            [ ( "active", model.tab == ParsedTree )
            ]
        ]
        [ text "Source tree" ]
    , button
        [ onClick <| Select SimplifiedTree
        , classList
            [ ( "active", model.tab == SimplifiedTree )
            ]
        ]
        [ text "Simplified tree" ]
    , button
        [ onClick <| Select SimplifiedCode
        , classList
            [ ( "active", model.tab == SimplifiedCode )
            ]
        ]
        [ text "Simplified code" ]
    ]


tabView : Model -> List (Html msg)
tabView model =
    case model.tab of
        ParserOutput ->
            case model.parsed of
                Err e ->
                    printErr model.content e
                        |> List.singleton

                _ ->
                    []

        ParsedTree ->
            model.parsed
                |> Result.map (List.singleton << visualizeExpr)
                |> Result.withDefault []

        ParsedCode ->
            model.parsed
                |> Result.map (List.singleton << text << Expr.toSource)
                |> Result.withDefault []

        SimplifiedTree ->
            model.simplified
                |> Maybe.map (List.singleton << visualizeExpr)
                |> Maybe.withDefault []

        SimplifiedCode ->
            model.simplified
                |> Maybe.map (List.singleton << text << Expr.toSource)
                |> Maybe.withDefault []


parser : Model -> Html msg
parser model =
    div [] <| tabView model


printErr : String -> List Parser.DeadEnd -> Html.Html msg
printErr source errors =
    let
        err =
            List.head errors
                |> Maybe.withDefault (Parser.DeadEnd 0 0 (Parser.Problem "Unknown problem"))
    in
    [ source, String.repeat (err.col - 1) " " ++ "^", Debug.toString err.problem ]
        |> String.join "\n"
        |> Html.text
        |> List.singleton
        |> Html.pre []


visualizeExpr : Expr -> Html msg
visualizeExpr e =
    div []
        [ text <| Expr.niceName e
        , Expr.arguments e
            |> List.map (\arg -> visualizeExpr arg)
            |> div [ style "padding-left" "1em", style "border-left" "2px solid gray" ]
        ]
