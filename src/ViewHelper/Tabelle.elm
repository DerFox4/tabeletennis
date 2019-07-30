module ViewHelper.Tabelle exposing (Tabelle, TabellenInfos, berechneSatzverhaeltnis, init, sortiere, update, view)

import Dict
import Html exposing (Html, table, td, text, th, tr)
import List.Extra
import Types.ProjectTyps as ProjectTyps exposing (Ergebnis, Game(..), Spieler, SpielerID, Turnier, defaultPlayer)
import ViewHelper.MyFontAwesome as MyFontAwesome


type alias Tabelle =
    List ( SpielerID, TabellenInfos )


type alias TabellenInfos =
    { spieler : Spieler
    , siege : Int
    , niederlagen : Int
    , absolvierteSpiele : Int
    , satzinfos : SatzInfos
    }


type alias SatzInfos =
    { gewonneneSätze : Int
    , verloreneSätze : Int
    , billanz : Int
    }


init : List Spieler -> Tabelle
init alleSpieler =
    List.map (\spieler -> ( spieler.id, TabellenInfos spieler 0 0 0 (SatzInfos 0 0 0) )) alleSpieler


sortiere : Tabelle -> Tabelle
sortiere tabelle =
    tabelle
        |> List.sortBy (\( _, tabelleninfos ) -> tabelleninfos.siege)
        |> List.Extra.groupWhile
            (\( _, tabelleninfos ) ( _, tabelleninfos2 ) ->
                tabelleninfos.siege == tabelleninfos2.siege
            )
        |> (\list ->
                List.map
                    (\( a, listA ) ->
                        a :: listA
                    )
                    list
           )
        |> List.map (List.sortBy (\( _, tabelleninfos ) -> tabelleninfos.satzinfos.billanz))
        |> List.concat
        |> List.reverse


update : Turnier -> List Spieler -> Tabelle
update turnier teilnehmer =
    List.map
        (\spieltag ->
            List.map
                (\game ->
                    game
                )
                spieltag.spieleAmSpieltag
        )
        turnier
        |> List.concat
        |> List.filter
            (\game ->
                case game of
                    OhneErgebnis _ _ ->
                        False

                    MitErgebnis _ _ _ ->
                        True
            )
        |> List.map
            (\game ->
                case game of
                    OhneErgebnis _ _ ->
                        Ergebnis defaultPlayer defaultPlayer [] 0

                    MitErgebnis _ _ ergebnis ->
                        ergebnis
            )
        |> List.foldl updateHelp (init teilnehmer)


updateHelp : Ergebnis -> Tabelle -> Tabelle
updateHelp ergebnis tabelle =
    let
        dict =
            Dict.fromList tabelle
    in
    dict
        |> Dict.update ergebnis.gewinner.id (updateInformationenFuerGewinner (ergebnis.gespielteSätze - 3))
        |> Dict.update ergebnis.verlierer.id (updateInformationenFuerVerlierer (ergebnis.gespielteSätze - 3))
        |> Dict.toList


updateInformationenFuerGewinner : Int -> Maybe TabellenInfos -> Maybe TabellenInfos
updateInformationenFuerGewinner verloreneSaetze tabellenInfos =
    case tabellenInfos of
        Nothing ->
            Nothing

        Just tabellenInfo ->
            Just (TabellenInfos tabellenInfo.spieler (tabellenInfo.siege + 1) tabellenInfo.niederlagen (tabellenInfo.absolvierteSpiele + 1) (berechneSatzverhaeltnis (tabellenInfo.satzinfos.gewonneneSätze + 3) (tabellenInfo.satzinfos.verloreneSätze + verloreneSaetze)))


updateInformationenFuerVerlierer : Int -> Maybe TabellenInfos -> Maybe TabellenInfos
updateInformationenFuerVerlierer gewonneneSatze tabellenInfos =
    case tabellenInfos of
        Nothing ->
            Nothing

        Just tabellenInfo ->
            Just (TabellenInfos tabellenInfo.spieler tabellenInfo.siege (tabellenInfo.niederlagen + 1) (tabellenInfo.absolvierteSpiele + 1) (berechneSatzverhaeltnis (tabellenInfo.satzinfos.gewonneneSätze + gewonneneSatze) (tabellenInfo.satzinfos.verloreneSätze + 3)))


berechneSatzverhaeltnis : Int -> Int -> SatzInfos
berechneSatzverhaeltnis gewonneneSatze verloreneSaetze =
    SatzInfos
        gewonneneSatze
        verloreneSaetze
        (gewonneneSatze - verloreneSaetze)


view : Tabelle -> Html msg
view tabelle =
    table
        []
        (viewHeader :: List.map (\( _, tabellenInfos ) -> viewZeile tabellenInfos) tabelle)


viewHeader : Html msg
viewHeader =
    tr []
        [ th [] [ MyFontAwesome.user ]
        , th [] [ MyFontAwesome.tabletennis ]
        , th [] [ MyFontAwesome.win ]
        , th [] [ MyFontAwesome.lose ]
        , th [] [ MyFontAwesome.chartBar ]
        ]


viewZeile : TabellenInfos -> Html msg
viewZeile tabellenInfos =
    tr []
        [ th [] [ text tabellenInfos.spieler.name ]
        , td [] [ text (String.fromInt tabellenInfos.absolvierteSpiele) ]
        , td [] [ text (String.fromInt tabellenInfos.siege) ]
        , td [] [ text (String.fromInt tabellenInfos.niederlagen) ]
        , td [] [ text (String.fromInt tabellenInfos.satzinfos.gewonneneSätze ++ " : " ++ String.fromInt tabellenInfos.satzinfos.verloreneSätze) ]
        ]
