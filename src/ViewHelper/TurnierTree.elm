module ViewHelper.TurnierTree exposing (Phase(..), berechneSpielpaarungen, berechneStartPhase, erstelleErstePhase, getGewinner, mergeTree, theirToList, toList)

import Array
import Canopy exposing (Node(..))
import Types.ProjectTyps as ProjectTyps exposing (Ergebnis, Game(..), Spiel, Spieler)


type Phase
    = Finale
    | Halbfinale
    | Viertelfinale
    | Achtelfinale
    | BestOf32
    | BestOf64
    | BestOf128
    | BestOf256


berechneStartPhase : Int -> Phase
berechneStartPhase int =
    if int < 3 then
        Finale

    else if int < 5 then
        Halbfinale

    else if int < 9 then
        Viertelfinale

    else if int < 17 then
        Achtelfinale

    else if int < 33 then
        BestOf32

    else if int < 65 then
        BestOf64

    else if int < 129 then
        BestOf128

    else
        BestOf256


erstelleErstePhase : List Spieler -> List (Node Game)
erstelleErstePhase teilnehmer =
    berechneSpielpaarungen teilnehmer
        |> List.indexedMap
            (\int ( first, second ) ->
                if first == ProjectTyps.defaultPlayer then
                    Canopy.leaf (MitErgebnis (int + 1) (Spiel first second) (Ergebnis second first [] 0))

                else if second == ProjectTyps.defaultPlayer then
                    Canopy.leaf (MitErgebnis (int + 1) (Spiel first second) (Ergebnis first second [] 0))

                else
                    Canopy.leaf (OhneErgebnis (int + 1) (Spiel first second))
            )


berechneSpielpaarungen : List Spieler -> List ( Spieler, Spieler )
berechneSpielpaarungen teilnehmer =
    let
        arrayTeilnehmer =
            Array.fromList teilnehmer

        arrayTeilnehmerSortiert =
            berechneSpielpaarungenHelp (berechneStartPhase (List.length teilnehmer)) Finale [ 1, 2 ]
                |> List.map
                    (\index ->
                        Array.get (index - 1) arrayTeilnehmer |> ProjectTyps.maybeDefaultPlayer
                    )
                |> Array.fromList
    in
    arrayTeilnehmerSortiert
        |> Array.indexedMap
            (\index _ ->
                if modBy 2 index == 0 then
                    ( Array.get index arrayTeilnehmerSortiert
                        |> ProjectTyps.maybeDefaultPlayer
                    , Array.get (index + 1) arrayTeilnehmerSortiert
                        |> ProjectTyps.maybeDefaultPlayer
                    )

                else
                    ( ProjectTyps.defaultPlayer, ProjectTyps.defaultPlayer )
            )
        |> Array.toList
        |> List.filter (\tuple -> not (Tuple.first tuple == ProjectTyps.defaultPlayer && Tuple.second tuple == ProjectTyps.defaultPlayer))


berechneSpielpaarungenHelp : Phase -> Phase -> List Int -> List Int
berechneSpielpaarungenHelp zielPhase aktuellePhase liste =
    if aktuellePhase == zielPhase then
        liste

    else
        List.indexedMap
            (\index element ->
                case aktuellePhase of
                    BestOf256 ->
                        [ element ]

                    _ ->
                        if modBy 2 index == 0 then
                            [ element, (List.length liste * 2 + 1) - element ]

                        else
                            [ (List.length liste * 2 + 1) - element, element ]
            )
            liste
            |> List.concat
            |> berechneSpielpaarungenHelp zielPhase (geheInDieNaechstePhase aktuellePhase)


geheInDieNaechstePhase : Phase -> Phase
geheInDieNaechstePhase phase =
    case phase of
        Finale ->
            Halbfinale

        Halbfinale ->
            Viertelfinale

        Viertelfinale ->
            Achtelfinale

        Achtelfinale ->
            BestOf32

        BestOf32 ->
            BestOf64

        BestOf64 ->
            BestOf128

        BestOf128 ->
            BestOf256

        BestOf256 ->
            BestOf256


mergeTree : List (Node Game) -> Node Game
mergeTree leafs =
    case leafs of
        first :: [] ->
            first

        _ ->
            mergeTwoTrees leafs
                |> mergeTree


mergeTwoTrees : List (Node Game) -> List (Node Game)
mergeTwoTrees list =
    case list of
        first :: second :: tail ->
            let
                spielNrFirst =
                    case first of
                        Node game _ ->
                            case game of
                                OhneErgebnis spielNr _ ->
                                    spielNr

                                MitErgebnis spielNr _ _ ->
                                    spielNr

                spielNrSecond =
                    case second of
                        Node game _ ->
                            case game of
                                OhneErgebnis spielNr _ ->
                                    spielNr

                                MitErgebnis spielNr _ _ ->
                                    spielNr
            in
            tail ++ List.singleton (Canopy.node (OhneErgebnis (spielNrFirst + spielNrSecond + 1000) (Spiel (getGewinner first) (getGewinner second))) [ first, second ])

        _ ->
            list


getGewinner : Node Game -> Spieler
getGewinner node =
    case node of
        Node game _ ->
            case game of
                OhneErgebnis _ _ ->
                    ProjectTyps.defaultPlayer

                MitErgebnis _ _ ergebnis ->
                    ergebnis.gewinner


toList : Node Game -> List Game
toList node =
    case node of
        Node game children ->
            case children of
                [] ->
                    [ game ]

                first :: second :: [] ->
                    [ toList first, [ game ], toList second ] |> List.concat

                _ ->
                    []


theirToList : Node Game -> List ( Game, Maybe Game )
theirToList node =
    case node of
        Node game children ->
            case children of
                [] ->
                    [ ( game, Canopy.parent game node |> Maybe.map (\(Canopy.Node v c) -> v) ) ]

                first :: second :: [] ->
                    [ [ ( game, Canopy.parent game node |> Maybe.map (\(Canopy.Node v c) -> v) ) ], theirToList first, theirToList second ] |> List.concat

                _ ->
                    []
