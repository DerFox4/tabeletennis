module ViewHelper.SiegerEhrung exposing (viewSiegerEhrung)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Types.ProjectTyps as ProjectTyps exposing (Spieler)
import ViewHelper.MyFontAwesome as MyFontAwesome


viewSiegerEhrung : List Spieler -> Html msg
viewSiegerEhrung list =
    div [ class "Formular" ]
        [ viewWinner (List.head list |> ProjectTyps.maybeDefaultPlayer)
        , viewSecond (List.drop 1 list |> List.head |> ProjectTyps.maybeDefaultPlayer)
        , viewThird (List.drop 2 list)
        ]


viewWinner : Spieler -> Html msg
viewWinner spieler =
    if spieler /= ProjectTyps.defaultPlayer then
        div [ class "Erster" ] [ MyFontAwesome.winner, text ("Der Gewinner dieser Spielklasse ist " ++ spieler.name ++ "!"), MyFontAwesome.winner ]

    else
        div [] []


viewSecond : Spieler -> Html msg
viewSecond spieler =
    if spieler /= ProjectTyps.defaultPlayer then
        div [ class "Zweiter" ] [ MyFontAwesome.second, text ("Der Zweitplazierte dieser Spielklasse ist " ++ spieler.name ++ "!"), MyFontAwesome.second ]

    else
        div [] []


viewThird : List Spieler -> Html msg
viewThird list =
    let
        filterList =
            List.filter (\spieler -> spieler /= ProjectTyps.defaultPlayer) list
    in
    if List.length filterList == 1 then
        div [ class "Dritter" ] [ MyFontAwesome.third, text ("Dritter wurde der Spieler " ++ (List.map .name filterList |> String.concat)), MyFontAwesome.third ]

    else if List.isEmpty filterList then
        div [ class "Dritter" ] [ MyFontAwesome.third, text ("Dritter wurden die Spieler " ++ (List.map .name filterList |> String.join " und ")), MyFontAwesome.third ]

    else
        div [] []
