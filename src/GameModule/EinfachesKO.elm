module GameModule.EinfachesKO exposing
    ( Model
    , Msg(..)
    , bestimmeRunden
    , init
    , istTurnierBeendet
    , treeFirstUpdate
    , treeSecondUpdate
    , update
    , view
    )

import Canopy exposing (Node(..))
import FunctionHelper.SpielEingabe as SpielEingabe exposing (Msg(..))
import Html exposing (Html, button, div, hr, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types.ProjectTyps as ProjectTyps exposing (Ergebnis, Game(..), Spiel, Spieler)
import ViewHelper.MyFontAwesome as MyFontAwesome
import ViewHelper.SiegerEhrung as SiegerEhrung
import ViewHelper.TurnierTree as TurnierTree exposing (Phase(..))


type Msg
    = StarteSpielEingabe Int Game
    | UpdateSpielEingabe SpielEingabe.Model SpielEingabe.Msg


type alias Runde =
    Int


type AktivesFenster
    = SpielEingabe SpielEingabe.Model
    | KOPhase



{- Ein paar Stichpunkte:


   Bei Ausgabe des Baum:
   Schleife
        x = Runde Nr.                                                   (Start bei 1, falls nicht teil der berechneten Runde, dann ++)
        y = 2^x (ist das Ergebnis für die Modulo-Rechnung)
        z = ((y/2) -1) Das Ergebnis falls es ein Spiel der Runde ist

   gerrechnet wird: Jedes Spiel aus einer Indexedmap (Start bei 0) wird mit dem Index berrechnet:
   Spiel 0 wird geprüft durch die Schleife bis das erste mal fündig, dieser Phase gehört das Spiel an. (Phase ist = x)

   Beispiel:
                                                        Spielnr.  x   y   Spielnr.                y          z
                                                           |      |   |     |                     |          |
                            Spiel Nr. 1 (Runde 1 -> x = 1) 0 % (2^1 = 2) -> 0 % 2 = 0 == z (z = ((2/2) -1) = 0 ) -> 0 == 0 Spiel gehört zu Runde 1
          3                 Spiel Nr. 2 (Runde 1 -> x = 1) 1 % (2^1 = 2) -> 1 % 2 = 1 == z (z = ((2/2) -1) = 0 ) -> 0 == 1 Spiel gehört nicht zu Runde 1
      1       5             Spiel Nr. 2 (Runde 2 -> x = 2) 1 % (2^2 = 4) -> 1 % 4 = 1 == z (z = ((4/2) -1) = 1 ) -> 1 == 1 Spiel gehört zu Runde 2
    0   2   4   6           Spiel Nr. 3 (Runde 1 -> x = 1) 2 % (2^1 = 2) -> 2 % 2 = 0 == z (z = ((2/2) -1) = 0 ) -> 0 == 0 Spiel gehört zu Runde 1
                            Spiel Nr. 4 (Runde 1 -> x = 1) 3 % (2^1 = 2) -> 3 % 2 = 1 == z (z = ((2/2) -1) = 0 ) -> 0 == 1 Spiel gehört nicht zu Runde 1
                            Spiel Nr. 4 (Runde 2 -> x = 2) 3 % (2^2 = 4) -> 3 % 4 = 3 == z (z = ((4/2) -1) = 1 ) -> 3 == 1 Spiel gehört nicht zu Runde 2
                            Spiel Nr. 4 (Runde 3 -> x = 3) 3 % (2^3 = 8) -> 3 % 8 = 8 == z (z = ((8/2) -1) = 3 ) -> 3 == 3 Spiel gehört zu Runde 3
                            usw...
-}


bestimmeRunden : List Game -> List ( Runde, Game )
bestimmeRunden games =
    List.indexedMap berrechneRundeFuerSpiel games


berrechneRundeFuerSpiel : Int -> Game -> ( Runde, Game )
berrechneRundeFuerSpiel prefixListIndex game =
    berrechneRundeHelp prefixListIndex 1
        |> (\runde -> ( runde, game ))


berrechneRundeHelp : Int -> Int -> Runde
berrechneRundeHelp prefixListIndex runde =
    let
        x =
            runde

        y =
            2 ^ x

        z =
            y // 2 - 1
    in
    if z == modBy y prefixListIndex then
        runde

    else
        berrechneRundeHelp prefixListIndex (runde + 1)


type alias Model =
    { teilnehmer : List Spieler
    , phase : Phase
    , aktivesFenster : AktivesFenster
    , tree : Canopy.Node Game
    , games : List ( Runde, Game )
    }


init : List Spieler -> Model
init teilnehmer =
    let
        erstelleTurnierbaumNeu =
            teilnehmer
                |> List.sortBy (\spieler -> spieler.punkte)
                |> List.reverse
                |> TurnierTree.erstelleErstePhase
                |> TurnierTree.mergeTree
    in
    { teilnehmer = teilnehmer
    , phase = TurnierTree.berechneStartPhase (List.length teilnehmer)
    , aktivesFenster = KOPhase
    , tree = erstelleTurnierbaumNeu
    , games =
        erstelleTurnierbaumNeu
            |> TurnierTree.toList
            |> bestimmeRunden
            |> List.sortBy (\( runde, _ ) -> runde)
            |> List.reverse
    }


view : Model -> Html Msg
view model =
    div []
        [ case model.aktivesFenster of
            KOPhase ->
                if istTurnierBeendet model.tree then
                    SiegerEhrung.viewSiegerEhrung (erstelleListeFuerSiegerehrung model.tree)

                else
                    viewGames model.games

            SpielEingabe modelSpielEingabe ->
                Html.map (updateSpielEingabe modelSpielEingabe) (SpielEingabe.view modelSpielEingabe)
        ]


viewGames : List ( Runde, Game ) -> Html Msg
viewGames gameDetails =
    let
        lists x =
            List.filter (\( runde, _ ) -> runde == x) (List.reverse gameDetails) |> List.map (\( _, game ) -> viewGame game)
    in
    List.map
        (\thisList ->
            div []
                [ div [ class "KOBaum" ] thisList
                , if List.isEmpty thisList then
                    div [] []

                  else
                    hr [] []
                ]
        )
        (List.map (\int -> lists int) (List.range 1 8 |> List.reverse))
        |> div []


viewGame : Game -> Html Msg
viewGame game =
    case game of
        OhneErgebnis spielNr spiel ->
            div [ class "Ergebnis" ]
                [ div [] [ text (spiel.spielerEins.name ++ " : " ++ spiel.spielerZwei.name) ]
                , if spiel.spielerEins == ProjectTyps.defaultPlayer || spiel.spielerZwei == ProjectTyps.defaultPlayer then
                    div [] []

                  else
                    button [ class "FontAwesomeBoxButton", onClick (StarteSpielEingabe spielNr game) ] [ MyFontAwesome.edit ]
                ]

        MitErgebnis _ _ ergebnis ->
            div []
                [ div [ class "Ergebnis" ]
                    [ div [ class "Gewinner" ] [ text ergebnis.gewinner.name ], div [] [ text " : " ], div [ class "Verlierer" ] [ text ergebnis.verlierer.name ] ]
                , div [] (List.map viewSatz ergebnis.sätze)
                ]


viewSatz : ( Int, Int ) -> Html Msg
viewSatz ( punkteSpielerEins, punkteSpielerZwei ) =
    div [ class "Satz" ] [ text (" " ++ String.fromInt punkteSpielerEins ++ " : " ++ String.fromInt punkteSpielerZwei ++ " ") ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        StarteSpielEingabe spielNr game ->
            { model
                | aktivesFenster =
                    SpielEingabe (SpielEingabe.init spielNr game)
            }

        UpdateSpielEingabe modelSpielEingabe msgSpielEingabe ->
            case msgSpielEingabe of
                Close ->
                    { model
                        | games =
                            treeFirstUpdate modelSpielEingabe.gameToUpdate modelSpielEingabe.endstand model.tree
                                |> treeSecondUpdate modelSpielEingabe.endstand
                                |> TurnierTree.toList
                                |> bestimmeRunden
                                |> List.sortBy (\( runde, _ ) -> runde)
                                |> List.reverse
                        , tree =
                            treeFirstUpdate modelSpielEingabe.gameToUpdate modelSpielEingabe.endstand model.tree
                                |> treeSecondUpdate modelSpielEingabe.endstand
                        , aktivesFenster = KOPhase
                    }

                _ ->
                    { model | aktivesFenster = SpielEingabe (SpielEingabe.update msgSpielEingabe modelSpielEingabe) }


updateSpielEingabe : SpielEingabe.Model -> SpielEingabe.Msg -> Msg
updateSpielEingabe model msg =
    UpdateSpielEingabe model msg


treeFirstUpdate : Game -> Game -> Node Game -> Node Game
treeFirstUpdate gameToUpdate updatedGame tree =
    Canopy.updateValueAt gameToUpdate (always updatedGame) tree


treeSecondUpdate : Game -> Node Game -> Node Game
treeSecondUpdate updatedGame updatedTree =
    let
        toUpdateParent : ( Game, List (Node Game) )
        toUpdateParent =
            Canopy.parent updatedGame updatedTree
                |> Maybe.map
                    (\node ->
                        case node of
                            Node game children ->
                                ( game, children )
                    )
                |> Maybe.withDefault ( OhneErgebnis 0 (Spiel ProjectTyps.defaultPlayer ProjectTyps.defaultPlayer), [] )

        spielNr =
            case Tuple.first toUpdateParent of
                OhneErgebnis thisSpielNr _ ->
                    thisSpielNr

                MitErgebnis thisSpielNr _ _ ->
                    thisSpielNr

        gewinnerFirst : Spieler
        gewinnerFirst =
            case Tuple.second toUpdateParent of
                first :: _ :: [] ->
                    TurnierTree.getGewinner first

                _ ->
                    ProjectTyps.defaultPlayer

        gewinnerSecond : Spieler
        gewinnerSecond =
            case Tuple.second toUpdateParent of
                _ :: second :: [] ->
                    TurnierTree.getGewinner second

                _ ->
                    ProjectTyps.defaultPlayer
    in
    Canopy.updateValueAt (Tuple.first toUpdateParent) (always (OhneErgebnis spielNr (Spiel gewinnerFirst gewinnerSecond))) updatedTree


istTurnierBeendet : Node Game -> Bool
istTurnierBeendet node =
    Canopy.all
        (\game ->
            case game of
                OhneErgebnis _ _ ->
                    False

                MitErgebnis _ _ _ ->
                    True
        )
        node


erstelleListeFuerSiegerehrung : Node Game -> List Spieler
erstelleListeFuerSiegerehrung node =
    case node of
        Node game children ->
            let
                turnierGewinner =
                    case game of
                        OhneErgebnis _ _ ->
                            ProjectTyps.defaultPlayer

                        MitErgebnis _ _ ergebnis ->
                            ergebnis.gewinner

                zweiter =
                    case game of
                        OhneErgebnis _ _ ->
                            ProjectTyps.defaultPlayer

                        MitErgebnis _ _ ergebnis ->
                            ergebnis.verlierer

                dritter : List Spieler
                dritter =
                    List.map
                        (\thisNode ->
                            case thisNode of
                                Node thisGame _ ->
                                    case thisGame of
                                        OhneErgebnis _ _ ->
                                            ProjectTyps.defaultPlayer

                                        MitErgebnis _ _ ergebnis ->
                                            ergebnis.verlierer
                        )
                        children
                        |> List.filter (\spieler -> spieler /= ProjectTyps.defaultPlayer)
            in
            [ turnierGewinner, zweiter ] ++ dritter
