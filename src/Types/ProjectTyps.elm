module Types.ProjectTyps exposing (Ergebnis, Game(..), QTTR, Satz, SatzInfos, Spiel, Spieler, SpielerID, Spieltag, Turnier, defaultPlayer, maybeDefaultPlayer)


type alias Turnier =
    List Spieltag


type alias Spieltag =
    { spieleAmSpieltag : List Game
    , spieltagNr : Int
    }


type alias SatzInfos =
    { gewonneneSätze : Int
    , verloreneSätze : Int
    , billanz : Int
    }


type Game
    = OhneErgebnis SpielNr Spiel
    | MitErgebnis SpielNr Spiel Ergebnis


type alias Spiel =
    { spielerEins : Spieler
    , spielerZwei : Spieler
    }


type alias Spieler =
    { name : String
    , punkte : QTTR
    , id : SpielerID
    }


type alias QTTR =
    Int


type alias SpielerID =
    Int


type alias Ergebnis =
    { gewinner : Spieler
    , verlierer : Spieler
    , sätze : List Satz
    , gespielteSätze : Int
    }


type alias Satz =
    ( Int, Int )


type alias SpielNr =
    Int


defaultPlayer : Spieler
defaultPlayer =
    Spieler "defaultPlayer" 0 0


maybeDefaultPlayer : Maybe Spieler -> Spieler
maybeDefaultPlayer maybeSpieler =
    Maybe.withDefault defaultPlayer maybeSpieler
