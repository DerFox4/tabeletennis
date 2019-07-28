module Example exposing (suite)

import Canopy
import Expect exposing (Expectation)
import GameModule.AlleGegenAlle exposing (erstelleSpieltagNr)
import GameModule.EinfachesKO
import Test exposing (..)
import Types.ProjectTyps exposing (Ergebnis, Game(..), Spiel, Spieler, Spieltag, defaultPlayer, maybeDefaultPlayer)
import ViewHelper.TurnierTree


suite : Test
suite =
    describe "Tischtennis"
        [ describe "AlleGegenAlle"
            [ describe "Spieltag"
                [ describe "gerade Anzahl an Spielern"
                    [ test "erster Spieltag 6 Spieler" <|
                        \() ->
                            erstelleSpieltagNr [ Spieler "Eins" 0 0, Spieler "Zwei" 0 0, Spieler "Drei" 0 0, Spieler "Vier" 0 0, Spieler "Fünf" 0 0, Spieler "Sechs" 0 0 ] 1
                                |> Expect.equal
                                    (Spieltag
                                        [ OhneErgebnis 1 (Spiel (Spieler "Zwei" 0 0) (Spieler "Fünf" 0 0))
                                        , OhneErgebnis 2 (Spiel (Spieler "Drei" 0 0) (Spieler "Vier" 0 0))
                                        , OhneErgebnis 3 (Spiel (Spieler "Eins" 0 0) (Spieler "Sechs" 0 0))
                                        ]
                                        1
                                    )
                    , test "dritter Spieltag 6 Spieler" <|
                        \() ->
                            erstelleSpieltagNr [ Spieler "Eins" 0 0, Spieler "Zwei" 0 0, Spieler "Drei" 0 0, Spieler "Vier" 0 0, Spieler "Fünf" 0 0, Spieler "Sechs" 0 0 ] 3
                                |> Expect.equal
                                    (Spieltag
                                        [ OhneErgebnis 1 (Spiel (Spieler "Vier" 0 0) (Spieler "Zwei" 0 0))
                                        , OhneErgebnis 2 (Spiel (Spieler "Fünf" 0 0) (Spieler "Eins" 0 0))
                                        , OhneErgebnis 3 (Spiel (Spieler "Drei" 0 0) (Spieler "Sechs" 0 0))
                                        ]
                                        3
                                    )
                    , test "fünfter Spieltag 8 Spieler" <|
                        \() ->
                            erstelleSpieltagNr [ Spieler "Eins" 0 0, Spieler "Zwei" 0 0, Spieler "Drei" 0 0, Spieler "Vier" 0 0, Spieler "Fünf" 0 0, Spieler "Sechs" 0 0, Spieler "Sieben" 0 0, Spieler "Acht" 0 0 ] 5
                                |> Expect.equal
                                    (Spieltag
                                        [ OhneErgebnis 1 (Spiel (Spieler "Sechs" 0 0) (Spieler "Vier" 0 0))
                                        , OhneErgebnis 2 (Spiel (Spieler "Sieben" 0 0) (Spieler "Drei" 0 0))
                                        , OhneErgebnis 3 (Spiel (Spieler "Eins" 0 0) (Spieler "Zwei" 0 0))
                                        , OhneErgebnis 4 (Spiel (Spieler "Fünf" 0 0) (Spieler "Acht" 0 0))
                                        ]
                                        5
                                    )
                    ]
                , describe "ungerade Anzahl an Spielern"
                    [ test "erster Spieltag 5 Spieler" <|
                        \() ->
                            erstelleSpieltagNr [ Spieler "Eins" 0 0, Spieler "Zwei" 0 0, Spieler "Drei" 0 0, Spieler "Vier" 0 0, Spieler "Fünf" 0 0 ] 1
                                |> Expect.equal
                                    (Spieltag
                                        [ OhneErgebnis 1 (Spiel (Spieler "Zwei" 0 0) (Spieler "Fünf" 0 0))
                                        , OhneErgebnis 2 (Spiel (Spieler "Drei" 0 0) (Spieler "Vier" 0 0))
                                        ]
                                        1
                                    )
                    , test "vierter Spieltag 5 Spieler" <|
                        \() ->
                            erstelleSpieltagNr [ Spieler "Eins" 0 0, Spieler "Zwei" 0 0, Spieler "Drei" 0 0, Spieler "Vier" 0 0, Spieler "Fünf" 0 0 ] 4
                                |> Expect.equal
                                    (Spieltag
                                        [ OhneErgebnis 1 (Spiel (Spieler "Fünf" 0 0) (Spieler "Drei" 0 0))
                                        , OhneErgebnis 2 (Spiel (Spieler "Eins" 0 0) (Spieler "Zwei" 0 0))
                                        ]
                                        4
                                    )
                    , test "fünf Spieltag 9 Spieler" <|
                        \() ->
                            erstelleSpieltagNr [ Spieler "Eins" 0 0, Spieler "Zwei" 0 0, Spieler "Drei" 0 0, Spieler "Vier" 0 0, Spieler "Fünf" 0 0, Spieler "Sechs" 0 0, Spieler "Sieben" 0 0, Spieler "Acht" 0 0, Spieler "Neun" 0 0 ] 5
                                |> Expect.equal
                                    (Spieltag
                                        [ OhneErgebnis 1 (Spiel (Spieler "Sechs" 0 0) (Spieler "Vier" 0 0))
                                        , OhneErgebnis 2 (Spiel (Spieler "Sieben" 0 0) (Spieler "Drei" 0 0))
                                        , OhneErgebnis 3 (Spiel (Spieler "Acht" 0 0) (Spieler "Zwei" 0 0))
                                        , OhneErgebnis 4 (Spiel (Spieler "Neun" 0 0) (Spieler "Eins" 0 0))
                                        ]
                                        5
                                    )
                    ]
                ]
            ]
        , describe
            "Einfaches K.O."
            [ describe "Spielerliste"
                [ test "1,2" <|
                    \() ->
                        ViewHelper.TurnierTree.berechneSpielpaarungen [ Spieler "A" 0 0, Spieler "B" 0 0 ]
                            |> Expect.equal [ ( Spieler "A" 0 0, Spieler "B" 0 0 ) ]
                , test "1,2,3,4,5" <|
                    \() ->
                        ViewHelper.TurnierTree.berechneSpielpaarungen [ Spieler "A" 0 0, Spieler "B" 0 0, Spieler "C" 0 0, Spieler "D" 0 0, Spieler "E" 0 0 ]
                            |> Expect.equal [ ( Spieler "A" 0 0, defaultPlayer ), ( Spieler "E" 0 0, Spieler "D" 0 0 ), ( Spieler "C" 0 0, defaultPlayer ), ( defaultPlayer, Spieler "B" 0 0 ) ]
                , test "1,2,3,4" <|
                    \() ->
                        ViewHelper.TurnierTree.berechneSpielpaarungen [ Spieler "A" 0 0, Spieler "B" 0 0, Spieler "C" 0 0, Spieler "D" 0 0 ]
                            |> Expect.equal [ ( Spieler "A" 0 0, Spieler "D" 0 0 ), ( Spieler "C" 0 0, Spieler "B" 0 0 ) ]
                , test "1,2,3" <|
                    \() ->
                        ViewHelper.TurnierTree.berechneSpielpaarungen [ Spieler "A" 0 0, Spieler "B" 0 0, Spieler "C" 0 0 ]
                            |> Expect.equal [ ( Spieler "A" 0 0, defaultPlayer ), ( Spieler "C" 0 0, Spieler "B" 0 0 ) ]
                , test "1,2,3,4,5,6,7,8" <|
                    \() ->
                        ViewHelper.TurnierTree.berechneSpielpaarungen [ Spieler "A" 0 0, Spieler "B" 0 0, Spieler "C" 0 0, Spieler "D" 0 0, Spieler "E" 0 0, Spieler "F" 0 0, Spieler "G" 0 0, Spieler "H" 0 0 ]
                            |> Expect.equal [ ( Spieler "A" 0 0, Spieler "H" 0 0 ), ( Spieler "E" 0 0, Spieler "D" 0 0 ), ( Spieler "C" 0 0, Spieler "F" 0 0 ), ( Spieler "G" 0 0, Spieler "B" 0 0 ) ]
                ]
            , describe "Leafs"
                [ test "Max, Moritz, Mo, Mike" <|
                    \() ->
                        ViewHelper.TurnierTree.erstelleErstePhase [ Spieler "Max" 0 0, Spieler "Moritz" 0 0, Spieler "Mo" 0 0, Spieler "Mike" 0 0 ]
                            |> Expect.equal [ Canopy.Node (OhneErgebnis 1 (Spiel (Spieler "Max" 0 0) (Spieler "Mike" 0 0))) [], Canopy.Node (OhneErgebnis 2 (Spiel (Spieler "Mo" 0 0) (Spieler "Moritz" 0 0))) [] ]
                ]
            , describe "GamesListe"
                [ test "first" <|
                    \() ->
                        [ Spieler "A" 100 1, Spieler "B" 5 2, Spieler "C" 3 3 ]
                            |> List.sortBy (\spieler -> spieler.punkte)
                            |> List.reverse
                            |> ViewHelper.TurnierTree.erstelleErstePhase
                            |> ViewHelper.TurnierTree.mergeTree
                            |> ViewHelper.TurnierTree.toList
                            |> GameModule.EinfachesKO.bestimmeRunden
                            |> List.sortBy (\( runde, _ ) -> runde)
                            |> Expect.equal
                                [ ( 1, MitErgebnis 1 (Spiel (Spieler "A" 100 1) defaultPlayer) (Ergebnis (Spieler "A" 100 1) defaultPlayer [] 0) )
                                , ( 1, OhneErgebnis 2 (Spiel (Spieler "C" 3 3) (Spieler "B" 5 2)) )
                                , ( 2, OhneErgebnis 1003 (Spiel (Spieler "A" 100 1) defaultPlayer) )
                                ]
                , test "second" <|
                    \() ->
                        [ Spieler "A" 10 10, Spieler "B" 9 9, Spieler "C" 8 8, Spieler "D" 7 7, Spieler "E" 6 6, Spieler "F" 5 5, Spieler "G" 4 4, Spieler "H" 2 2 ]
                            |> List.sortBy (\spieler -> spieler.punkte)
                            |> List.reverse
                            |> ViewHelper.TurnierTree.erstelleErstePhase
                            |> ViewHelper.TurnierTree.mergeTree
                            |> ViewHelper.TurnierTree.toList
                            |> GameModule.EinfachesKO.bestimmeRunden
                            |> List.sortBy (\( runde, _ ) -> runde)
                            |> Expect.equal
                                [ ( 1, OhneErgebnis 1 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2)) )
                                , ( 1, OhneErgebnis 2 (Spiel (Spieler "E" 6 6) (Spieler "D" 7 7)) )
                                , ( 1, OhneErgebnis 3 (Spiel (Spieler "C" 8 8) (Spieler "F" 5 5)) )
                                , ( 1, OhneErgebnis 4 (Spiel (Spieler "G" 4 4) (Spieler "B" 9 9)) )
                                , ( 2, OhneErgebnis 1003 (Spiel defaultPlayer defaultPlayer) )
                                , ( 2, OhneErgebnis 1007 (Spiel defaultPlayer defaultPlayer) )
                                , ( 3, OhneErgebnis 3010 (Spiel defaultPlayer defaultPlayer) )
                                ]
                ]
            , describe "trySomething"
                [ test "sfas" <|
                    \() ->
                        ViewHelper.TurnierTree.toList (Canopy.Node (OhneErgebnis 1 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2))) [])
                            |> Expect.equal [ OhneErgebnis 1 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2)) ]
                , test "fas" <|
                    \() ->
                        ViewHelper.TurnierTree.toList
                            (Canopy.Node (OhneErgebnis 3 (Spiel defaultPlayer defaultPlayer))
                                [ Canopy.Node (OhneErgebnis 1 (Spiel defaultPlayer defaultPlayer))
                                    [ Canopy.Node (OhneErgebnis 0 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2))) []
                                    , Canopy.Node (OhneErgebnis 2 (Spiel (Spieler "E" 10 10) (Spieler "D" 2 2))) []
                                    ]
                                , Canopy.Node (OhneErgebnis 5 (Spiel defaultPlayer defaultPlayer))
                                    [ Canopy.Node (OhneErgebnis 4 (Spiel (Spieler "C" 10 10) (Spieler "F" 2 2))) []
                                    , Canopy.Node (OhneErgebnis 6 (Spiel (Spieler "G" 10 10) (Spieler "B" 2 2))) []
                                    ]
                                ]
                            )
                            |> Expect.equal
                                [ OhneErgebnis 0 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2))
                                , OhneErgebnis 1 (Spiel defaultPlayer defaultPlayer)
                                , OhneErgebnis 2 (Spiel (Spieler "E" 10 10) (Spieler "D" 2 2))
                                , OhneErgebnis 3 (Spiel defaultPlayer defaultPlayer)
                                , OhneErgebnis 4 (Spiel (Spieler "C" 10 10) (Spieler "F" 2 2))
                                , OhneErgebnis 5 (Spiel defaultPlayer defaultPlayer)
                                , OhneErgebnis 6 (Spiel (Spieler "G" 10 10) (Spieler "B" 2 2))
                                ]
                ]
            , describe "Turnier beendet"
                [ test "True" <|
                    \() ->
                        GameModule.EinfachesKO.istTurnierBeendet
                            (Canopy.Node (MitErgebnis 0 (Spiel defaultPlayer defaultPlayer) (Ergebnis (Spieler "A" 10 10) (Spieler "H" 2 2) [] 0))
                                [ Canopy.Node (MitErgebnis 0 (Spiel (Spieler "A" 10 10) (Spieler "E" 10 10)) (Ergebnis (Spieler "A" 10 10) (Spieler "H" 2 2) [] 0))
                                    [ Canopy.Node (MitErgebnis 0 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2)) (Ergebnis (Spieler "A" 10 10) (Spieler "H" 2 2) [] 0)) []
                                    , Canopy.Node (MitErgebnis 0 (Spiel (Spieler "E" 10 10) (Spieler "D" 2 2)) (Ergebnis (Spieler "E" 10 10) (Spieler "D" 2 2) [] 0)) []
                                    ]
                                , Canopy.Node (MitErgebnis 0 (Spiel defaultPlayer defaultPlayer) (Ergebnis (Spieler "E" 10 10) (Spieler "D" 2 2) [] 0))
                                    [ Canopy.Node (MitErgebnis 0 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2)) (Ergebnis (Spieler "A" 10 10) (Spieler "H" 2 2) [] 0)) []
                                    , Canopy.Node (MitErgebnis 0 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2)) (Ergebnis (Spieler "A" 10 10) (Spieler "H" 2 2) [] 0)) []
                                    ]
                                ]
                            )
                            |> Expect.equal True
                , test "False" <|
                    \() ->
                        GameModule.EinfachesKO.istTurnierBeendet
                            (Canopy.Node (OhneErgebnis 397 (Spiel defaultPlayer defaultPlayer))
                                [ Canopy.Node (OhneErgebnis 132 (Spiel (Spieler "A" 10 10) (Spieler "E" 10 10)))
                                    [ Canopy.Node (MitErgebnis 1 (Spiel (Spieler "A" 10 10) (Spieler "H" 2 2)) (Ergebnis (Spieler "A" 10 10) (Spieler "H" 2 2) [] 0)) []
                                    , Canopy.Node (MitErgebnis 2 (Spiel (Spieler "E" 10 10) (Spieler "D" 2 2)) (Ergebnis (Spieler "E" 10 10) (Spieler "D" 2 2) [] 0)) []
                                    ]
                                , Canopy.Node (OhneErgebnis 136 (Spiel defaultPlayer defaultPlayer))
                                    [ Canopy.Node (OhneErgebnis 3 (Spiel (Spieler "C" 10 10) (Spieler "F" 2 2))) []
                                    , Canopy.Node (OhneErgebnis 4 (Spiel (Spieler "G" 10 10) (Spieler "B" 2 2))) []
                                    ]
                                ]
                            )
                            |> Expect.equal False
                ]
            ]
        ]
