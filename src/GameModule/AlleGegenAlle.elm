module GameModule.AlleGegenAlle exposing (Model, Msg(..), erstelleSpieltagNr, init, update, view)

import Array exposing (Array)
import FunctionHelper.SpielEingabe as SpielEingabe exposing (Msg(..))
import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types.ProjectTyps as ProjectTyps exposing (Ergebnis, Game(..), Satz, Spiel, Spieler, Spieltag, Turnier, defaultPlayer, maybeDefaultPlayer)
import ViewHelper.MyFontAwesome as MyFontAwesome
import ViewHelper.SiegerEhrung as SiegerEhrung
import ViewHelper.Tabelle as Tabelle exposing (Tabelle, TabellenInfos)


type Fokus
    = Endstand
    | Turnierbaum


type alias SpieltagHelper =
    { spieleAmSpieltag : Array Game
    , spieltagNr : Int
    }


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
    , turnier = erstelleTurnier teilnehmer
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
                        oldturnierArrayGame : Array SpieltagHelper
                        oldturnierArrayGame =
                            List.map
                                (\spieltag ->
                                    SpieltagHelper (spieltag.spieleAmSpieltag |> Array.fromList) spieltag.spieltagNr
                                )
                                model.turnier
                                |> Array.fromList

                        oldSpieltag : Array Game
                        oldSpieltag =
                            oldturnierArrayGame
                                |> Array.get (modelSpielEingabe.spieltagNr - 1)
                                |> Maybe.withDefault (SpieltagHelper (Array.fromList []) 0)
                                |> (\spieltagHelper ->
                                        spieltagHelper.spieleAmSpieltag
                                   )

                        newSpieltag : Array Game
                        newSpieltag =
                            Array.set (modelSpielEingabe.spielNr - 1) modelSpielEingabe.endstand oldSpieltag

                        newTurnier : Turnier
                        newTurnier =
                            oldturnierArrayGame
                                |> Array.set
                                    (modelSpielEingabe.spieltagNr - 1)
                                    (SpieltagHelper newSpieltag modelSpielEingabe.spieltagNr)
                                |> transformArraySpieltagHelper
                    in
                    { model
                        | turnier = newTurnier
                        , aktivesFenster = LaufendesTurnier
                        , tabelle = Tabelle.update newTurnier model.teilnehmer |> Tabelle.sortiere
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


transformArraySpieltagHelper : Array SpieltagHelper -> Turnier
transformArraySpieltagHelper array =
    array
        |> Array.map
            (\helper ->
                Spieltag (helper.spieleAmSpieltag |> Array.toList) helper.spieltagNr
            )
        |> Array.toList


erstelleTurnier : List Spieler -> Turnier
erstelleTurnier listSpieler =
    let
        spieltage =
            if modBy 2 (List.length listSpieler) == 0 then
                List.range 1 (List.length listSpieler - 1)

            else
                List.range 1 (List.length listSpieler)
    in
    List.map
        (\spieltagNummer ->
            erstelleSpieltagNr listSpieler spieltagNummer
        )
        spieltage



{-
   Infos zum Algorithmus:
   n = falls gerade
           (Anzahl der Spieler - 1)
       sonst
           (Anzahl der Spieler)

   k : SpielNr innerhalb einer Runde
   k = (1 -> n//2)

   i : Anzahl der Spieltage
   i = 1 -> n

   SpielerEins = modBy n (i + k)
   SpielerZwei = modBy n (i - k)

   Sonderfall bei gerader Anzahl an Spielern:
   Letztes Spiel einer Runde : i gegen n
-}


erstelleSpieltagNr : List Spieler -> Int -> Spieltag
erstelleSpieltagNr listSpieler spieltagNr =
    let
        n =
            if modBy 2 (List.length listSpieler) == 0 then
                List.length listSpieler - 1

            else
                List.length listSpieler

        kWerte =
            List.range 1 (List.length listSpieler // 2)

        i =
            spieltagNr

        arraySpieler =
            defaultPlayer :: listSpieler |> Array.fromList

        spieltag =
            List.repeat (List.length kWerte) (OhneErgebnis 0 (Spiel defaultPlayer defaultPlayer))
    in
    Spieltag
        (List.map
            (\k ->
                let
                    playerOne =
                        if modBy n (i + k) == 0 then
                            n

                        else
                            modBy n (i + k)

                    playerTwo =
                        if modBy n (i - k) == 0 then
                            n

                        else
                            modBy n (i - k)
                in
                if k == List.length spieltag && modBy 2 (List.length listSpieler) == 0 then
                    OhneErgebnis
                        (List.length listSpieler // 2)
                        (Spiel
                            (Array.get i arraySpieler |> maybeDefaultPlayer)
                            (Array.get (n + 1) arraySpieler |> maybeDefaultPlayer)
                        )

                else
                    OhneErgebnis
                        k
                        (Spiel
                            (Array.get playerOne arraySpieler |> maybeDefaultPlayer)
                            (Array.get playerTwo arraySpieler |> maybeDefaultPlayer)
                        )
            )
            kWerte
        )
        i


view : Model -> Html Msg
view model =
    div []
        [ if turnierBeendet model.turnier then
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


turnierBeendet : Turnier -> Bool
turnierBeendet turnier =
    List.map
        (\spieltag ->
            List.map
                (\game ->
                    case game of
                        MitErgebnis _ _ _ ->
                            True

                        OhneErgebnis _ _ ->
                            False
                )
                spieltag.spieleAmSpieltag
        )
        turnier
        |> List.concat
        |> List.all (\game -> game == True)
