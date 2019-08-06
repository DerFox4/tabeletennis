module ViewHelper.GruppenTurnier exposing (Spieltag, Turnier, erstelleSpieltagNr, init, turnierBeendet, update)

import Array exposing (Array)
import Types.ProjectTyps exposing (Game(..), Spiel, Spieler, defaultPlayer, maybeDefaultPlayer)


type alias Turnier =
    List Spieltag


type alias Spieltag =
    { spieleAmSpieltag : List Game
    , spieltagNr : Int
    }


type alias SpieltagHelper =
    { spieleAmSpieltag : Array Game
    , spieltagNr : Int
    }


init : List Spieler -> Turnier
init listSpieler =
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


update : Turnier -> Game -> Int -> Int -> Turnier
update turnierBeforeUpdate endstand spieltagNr spielNr =
    let
        turnierBeforeUpdateArrayGame : Array SpieltagHelper
        turnierBeforeUpdateArrayGame =
            List.map
                (\spieltag ->
                    SpieltagHelper (spieltag.spieleAmSpieltag |> Array.fromList) spieltag.spieltagNr
                )
                turnierBeforeUpdate
                |> Array.fromList

        spieltagBeforeUpdate : Array Game
        spieltagBeforeUpdate =
            turnierBeforeUpdateArrayGame
                |> Array.get (spieltagNr - 1)
                |> Maybe.withDefault (SpieltagHelper (Array.fromList []) 0)
                |> (\spieltagHelper ->
                        spieltagHelper.spieleAmSpieltag
                   )

        spieltagAfterUpdate : Array Game
        spieltagAfterUpdate =
            Array.set (spielNr - 1) endstand spieltagBeforeUpdate
    in
    turnierBeforeUpdateArrayGame
        |> Array.set
            (spieltagNr - 1)
            (SpieltagHelper spieltagAfterUpdate spieltagNr)
        |> transformArraySpieltagHelper


transformArraySpieltagHelper : Array SpieltagHelper -> Turnier
transformArraySpieltagHelper array =
    array
        |> Array.map
            (\helper ->
                Spieltag (helper.spieleAmSpieltag |> Array.toList) helper.spieltagNr
            )
        |> Array.toList


turnierBeendet : Turnier -> Bool
turnierBeendet turnier =
    List.all
        (\spieltage ->
            List.all
                (\game ->
                    case game of
                        MitErgebnis _ _ _ ->
                            True

                        OhneErgebnis _ _ ->
                            False
                )
                spieltage.spieleAmSpieltag
        )
        turnier
