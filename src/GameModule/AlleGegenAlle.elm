module GameModule.AlleGegenAlle exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import FunctionHelper.SpielEingabe as SpielEingabe exposing (Msg(..))
import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types.ProjectTyps as ProjectTyps exposing (Ergebnis, Game(..), Satz, Spiel, Spieler)
import ViewHelper.GruppenTurnier as GruppenTurnier exposing (Spieltag, Turnier)
import ViewHelper.MyFontAwesome as MyFontAwesome
import ViewHelper.SiegerEhrung as SiegerEhrung
import ViewHelper.Tabelle as Tabelle exposing (Tabelle, TabellenInfos)


type Fokus
    = Endstand
    | Turnierbaum


type AktivesFenster
    = LaufendesTurnier
    | SpielEingabe SpielEingabe.Model


type alias Model =
    { aktivesFenster : AktivesFenster
    , turnier : Turnier
    , tabelle : Tabelle
    , fokusAuf : Maybe Fokus
    , teilnehmer : List Spieler
    }


type Msg
    = UpdateSpielEingabe SpielEingabe.Model SpielEingabe.Msg
    | StarteSpielEingabe Int Game
    | ZeigTabelle
    | ZeigTurnierbaum


init : List Spieler -> Model
init teilnehmer =
    { aktivesFenster = LaufendesTurnier
    , turnier = GruppenTurnier.init teilnehmer
    , tabelle = Tabelle.init teilnehmer
    , fokusAuf = Nothing
    , teilnehmer = teilnehmer
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSpielEingabe modelSpielEingabe msgSpielEingabe ->
            case msgSpielEingabe of
                Close ->
                    let
                        turnierAfterUpdate =
                            GruppenTurnier.update model.turnier modelSpielEingabe.endstand modelSpielEingabe.spieltagNr modelSpielEingabe.spielNr
                    in
                    { model
                        | turnier = turnierAfterUpdate
                        , aktivesFenster = LaufendesTurnier
                        , tabelle = Tabelle.update turnierAfterUpdate model.teilnehmer |> Tabelle.sortiere
                    }

                _ ->
                    { model | aktivesFenster = SpielEingabe (SpielEingabe.update msgSpielEingabe modelSpielEingabe) }

        StarteSpielEingabe spieltagNr game ->
            { model
                | aktivesFenster =
                    SpielEingabe (SpielEingabe.init spieltagNr game)
            }

        ZeigTabelle ->
            { model
                | fokusAuf =
                    if model.fokusAuf == Just Endstand then
                        Nothing

                    else
                        Just Endstand
            }

        ZeigTurnierbaum ->
            { model
                | fokusAuf =
                    if model.fokusAuf == Just Turnierbaum then
                        Nothing

                    else
                        Just Turnierbaum
            }


updateSpielEingabe : SpielEingabe.Model -> SpielEingabe.Msg -> Msg
updateSpielEingabe model msg =
    UpdateSpielEingabe model msg


view : Model -> Html Msg
view model =
    div []
        [ if GruppenTurnier.turnierBeendet model.turnier then
            div []
                [ SiegerEhrung.viewSiegerEhrung (erstelleSpielerListeFuerSiegerehrung model.tabelle)
                , div []
                    [ button [ onClick ZeigTabelle, class "FontAwesomeBoxButton" ]
                        [ case model.fokusAuf of
                            Just Endstand ->
                                MyFontAwesome.back

                            _ ->
                                MyFontAwesome.table
                        ]
                    , button [ onClick ZeigTurnierbaum, class "FontAwesomeBoxButton" ]
                        [ case model.fokusAuf of
                            Just Turnierbaum ->
                                MyFontAwesome.back

                            _ ->
                                MyFontAwesome.chartBar
                        ]
                    ]
                , div []
                    [ case model.fokusAuf of
                        Nothing ->
                            div [] []

                        Just Turnierbaum ->
                            viewTurnier model.turnier

                        Just Endstand ->
                            Tabelle.view model.tabelle
                    ]
                ]

          else
            div []
                [ case model.aktivesFenster of
                    SpielEingabe modelSpielEingabe ->
                        div []
                            [ Html.map (updateSpielEingabe modelSpielEingabe) (SpielEingabe.view modelSpielEingabe) ]

                    LaufendesTurnier ->
                        div [ class "TurnierBox" ]
                            [ div [ class "Turnier" ] [ viewTurnier model.turnier ]
                            , div [ class "Tabelle" ] [ Tabelle.view model.tabelle ]
                            ]
                ]
        ]


viewTurnier : Turnier -> Html Msg
viewTurnier turnier =
    div [] (List.map viewSpieltag turnier)


viewSpieltag : Spieltag -> Html Msg
viewSpieltag spieltag =
    div [ class "Spieltag" ]
        [ h3 [] [ text ("Spieltag " ++ String.fromInt spieltag.spieltagNr) ]
        , div [] (List.map (viewGame spieltag.spieltagNr) spieltag.spieleAmSpieltag)
        ]


viewGame : Int -> Game -> Html Msg
viewGame spieltagNr game =
    case game of
        OhneErgebnis _ spiel ->
            div [ class "OhneErgebnis" ]
                [ viewSpiel spiel
                , button [ onClick (StarteSpielEingabe spieltagNr game), class "FontAwesomeBoxButton" ]
                    [ MyFontAwesome.pencil ]
                ]

        MitErgebnis _ spiel ergebnis ->
            div [ class "MitErgebnis" ]
                [ div [] [ viewSpiel spiel ]
                , div [] [ viewErgebnis ergebnis.sätze ]
                , div [] [ viewGewinner ergebnis.gewinner ]
                , div []
                    [ button [ class "FontAwesomeBoxButton", onClick (StarteSpielEingabe spieltagNr game) ]
                        [ MyFontAwesome.edit ]
                    ]
                ]


viewSpiel : Spiel -> Html Msg
viewSpiel spiel =
    div [ class "Spiel" ] [ text (spiel.spielerEins.name ++ "(" ++ String.fromInt spiel.spielerEins.punkte ++ ")" ++ " : " ++ spiel.spielerZwei.name ++ "(" ++ String.fromInt spiel.spielerZwei.punkte ++ ")") ]


viewErgebnis : List Satz -> Html Msg
viewErgebnis saetze =
    div [ class "Sätze" ] (List.map viewSatz saetze)


viewGewinner : Spieler -> Html Msg
viewGewinner { name } =
    div [ class "Gewinner" ] [ text ("Gewinner: " ++ name) ]


viewSatz : ( Int, Int ) -> Html Msg
viewSatz ( punkteSpielerEins, punkteSpielerZwei ) =
    div [ class "Satz" ] [ text (" " ++ String.fromInt punkteSpielerEins ++ " : " ++ String.fromInt punkteSpielerZwei ++ " ") ]


erstelleSpielerListeFuerSiegerehrung : Tabelle -> List Spieler
erstelleSpielerListeFuerSiegerehrung tabelle =
    List.take 3 tabelle |> List.map (\( _, infos ) -> infos.spieler)
