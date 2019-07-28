module FunctionHelper.SpielerEingabe exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h2, h3, hr, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Types.ProjectTyps as ProjectTyps exposing (Spieler)
import ViewHelper.MyFontAwesome as MyFontAwesome


type alias Model =
    { listSpieler : List Spieler
    , name : String
    , qttr : Int
    , id : Int
    }


init : Model
init =
    { listSpieler = []
    , name = ""
    , qttr = 0
    , id = 1000
    }


type Msg
    = SpielerHinzufuegen Spieler
    | EingabeLoeschen
    | StarteTurnier
    | AktualisiereName String
    | AktualisierePunkte String


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Spielereingabe:" ]
        , h3 [] [ text "Hier können Sie nun einen Spieler nach dem anderen zur Teilnehmerliste hinzufügen:" ]
        , div [ class "Formular" ]
            [ div []
                [ text "Den Namen des Spielers: " ]
            , input [ onInput AktualisiereName ] []
            , div [] [ text "Letzten Q-TTR-Punkte: " ]
            , input [ onInput AktualisierePunkte ] []
            , div []
                [ case ( String.length model.name > 2, model.qttr > 100 ) of
                    ( True, True ) ->
                        div []
                            [ button [ class "FontAwesomeBoxButton", onClick (SpielerHinzufuegen (Spieler model.name model.qttr model.id)) ] [ MyFontAwesome.addUser ]
                            , button [ class "FontAwesomeBoxButton", onClick EingabeLoeschen ] [ MyFontAwesome.trash ]
                            ]

                    ( True, False ) ->
                        div [] [ text "Die Eingabe der Punkte ist bislang noch nicht korrekt" ]

                    ( False, True ) ->
                        div [] [ text "Bei der Eingabe des Namens ist etwas schiefgelaufen" ]

                    ( False, False ) ->
                        div [] [ text "Mit dieser Eingabe kann kein weiterer Spieler erstellt werden!" ]
                , if List.length model.listSpieler > 2 && model.name == "" && model.qttr == 0 then
                    div [] [ button [ onClick StarteTurnier ] [ text "Das Turnier starten" ] ]

                  else
                    div [] []
                ]
            ]
        , hr [] []
        , div []
            [ h3 [] [ text "Die aktuelle Teilnehmerliste:" ]
            , viewSpielerListe model.listSpieler
            ]
        ]


viewSpielerListe : List Spieler -> Html Msg
viewSpielerListe list =
    div [ class "Spielerliste" ]
        [ div [] (List.indexedMap (verteileSpieler 0) list)
        , div [] (List.indexedMap (verteileSpieler 1) list)
        , div [] (List.indexedMap (verteileSpieler 2) list)
        ]


verteileSpieler : Int -> Int -> Spieler -> Html Msg
verteileSpieler searchedIndex index spieler =
    if modBy 3 index == searchedIndex then
        viewSpieler spieler

    else
        div [] []


viewSpieler : Spieler -> Html Msg
viewSpieler spieler =
    text ("Name: " ++ spieler.name ++ " / QTTR: " ++ String.fromInt spieler.punkte ++ " / ID: " ++ String.fromInt spieler.id)


update : Msg -> Model -> Model
update msg model =
    case msg of
        SpielerHinzufuegen spieler ->
            { model
                | listSpieler =
                    spieler :: model.listSpieler
                , name = ""
                , qttr = 0
                , id = model.id + 1
            }

        EingabeLoeschen ->
            { model | name = "", qttr = 0 }

        StarteTurnier ->
            model

        AktualisiereName newName ->
            { model | name = newName }

        AktualisierePunkte newPunkte ->
            { model | qttr = String.toInt newPunkte |> Maybe.withDefault 0 }
