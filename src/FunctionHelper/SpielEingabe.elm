module FunctionHelper.SpielEingabe exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Html exposing (Html, button, div, h1, h2, h3, hr, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Types.ProjectTyps as ProjectTyps exposing (Ergebnis, Game(..), Satz, Spiel, Spieler)


type alias Model =
    { eingabeGültig : Bool
    , spiel : Array (Result String ( Int, Int ))
    , ergebnis : Maybe Ergebnis
    , spieltagNr : Int
    , spielNr : Int
    , gameToUpdate : Game
    , endstand : Game
    }


init : Int -> Game -> Model
init spieltagNr game =
    let
        spielNr =
            case game of
                MitErgebnis thisSpielNr _ _ ->
                    thisSpielNr

                OhneErgebnis thisSpielNr _ ->
                    thisSpielNr

        ergebnis =
            case game of
                MitErgebnis _ _ thisErgebnis ->
                    Just thisErgebnis

                OhneErgebnis _ _ ->
                    Nothing
    in
    { eingabeGültig = False
    , spiel =
        Array.fromList
            [ Err "Satz noch nicht eingetragen"
            , Err "Satz noch nicht eingetragen"
            , Err "Satz noch nicht eingetragen"
            , Err "Satz noch nicht eingetragen"
            , Err "Satz noch nicht eingetragen"
            ]
    , spieltagNr = spieltagNr
    , spielNr = spielNr
    , ergebnis = ergebnis
    , gameToUpdate = game
    , endstand = game
    }


type Msg
    = UpdateSatzEingabe Int String
    | ErgebnisseUebertragen
    | Close


view : Model -> Html Msg
view model =
    let
        spielerEins =
            case model.gameToUpdate of
                MitErgebnis _ spiel _ ->
                    spiel.spielerEins

                OhneErgebnis _ spiel ->
                    spiel.spielerEins

        spielerZwei =
            case model.gameToUpdate of
                MitErgebnis _ spiel _ ->
                    spiel.spielerZwei

                OhneErgebnis _ spiel ->
                    spiel.spielerZwei
    in
    div []
        [ h1 [] [ text (spielerEins.name ++ " : " ++ spielerZwei.name) ]
        , div [ class "Formular" ]
            [ div [] [ text "Ergebnis Satz 1: ", input [ onInput (UpdateSatzEingabe 0) ] [] ]
            , div [] [ text "Ergebnis Satz 2: ", input [ onInput (UpdateSatzEingabe 1) ] [] ]
            , div [] [ text "Ergebnis Satz 3: ", input [ onInput (UpdateSatzEingabe 2) ] [] ]
            , div [] [ text "Ergebnis Satz 4: ", input [ onInput (UpdateSatzEingabe 3) ] [] ]
            , div [] [ text "Ergebnis Satz 5: ", input [ onInput (UpdateSatzEingabe 4) ] [] ]
            , div []
                [ let
                    saetze =
                        model.spiel |> Array.toList

                    satzergebnisseValide =
                        saetze |> List.all istValide

                    spielergebnisLogisch =
                        saetze |> gewonnenHat |> pruefeAufErgebnisLogik

                    dieRichtigenSaetzeGespielt =
                        model.spiel |> richtigenSaetze
                  in
                  case ( satzergebnisseValide, spielergebnisLogisch, dieRichtigenSaetzeGespielt ) of
                    ( True, True, True ) ->
                        button [ onClick ErgebnisseUebertragen ] [ text "Ergebnis anzeigen lassen" ]

                    ( True, False, True ) ->
                        div [] [ text "Bei der Eingabe stimmt etwas mit der Anzahl der Sätze nicht!" ]

                    ( False, True, True ) ->
                        div [] [ text "Das Ergebnis von mindestens einem Satzes ist nicht logisch!" ]

                    ( True, True, False ) ->
                        div [] [ text "Die Ergebnisse ergeben in dieser Reihenfolge keinen Sinn" ]

                    _ ->
                        div [] [ text "Die Ergebnisse können so noch nicht übernommen werden!" ]
                , case model.ergebnis of
                    Nothing ->
                        button [ onClick Close ] [ text "Ohne Ergebnis zurück zum Turnier" ]

                    Just _ ->
                        div [] []
                ]
            ]
        , hr [] []
        , div []
            [ case model.ergebnis of
                Nothing ->
                    div [ class "Formular" ]
                        [ h2 [] [ text "Kurze Info für alle Einsteiger mit diesem Tool:" ]
                        , div []
                            [ div []
                                [ text "Die Eingabe kann auf zwei Arten Erfolgen:" ]
                            , div [ class "WithBorder" ]
                                [ div []
                                    [ text "Trennung durch Doppelpunkt: 11:2, 7:11, 13:15 11:4 99:97 " ]
                                , div []
                                    [ text "Durch Abkürzung des Ergebnis: 2, -7, -13, 4, 97" ]
                                ]
                            ]
                        ]

                Just ergebnis ->
                    div [ class "Formular" ]
                        [ h3 [] [ text "Stimmt diese Eingabe?" ]
                        , div []
                            [ div [] [ text ("Gewinner: " ++ ergebnis.gewinner.name) ]
                            , div [] [ text ("Verlierer: " ++ ergebnis.verlierer.name) ]
                            , div [] [ text ("Satzergebnisse: " ++ (viewSatzergebnisse ergebnis.sätze |> String.join ", ")) ]
                            ]
                        , button [ onClick Close ] [ text "Ergebnis in Tabelle übertragen" ]
                        , hr [] []
                        , div [] [ text "Sollte etwas nicht stimmen korrigieren Sie bitte ihre Eingabe in den Feldern!" ]
                        ]
            ]
        ]


viewSatzergebnisse : List Satz -> List String
viewSatzergebnisse list =
    List.map
        (\satz ->
            "(" ++ String.fromInt (Tuple.first satz) ++ ":" ++ String.fromInt (Tuple.second satz) ++ ")"
        )
        list


richtigenSaetze : Array (Result String Satz) -> Bool
richtigenSaetze array =
    let
        saetzeRichtig =
            [ Array.get 0 array |> Result.fromMaybe (Err "Da war kein Satz")
            , Array.get 1 array |> Result.fromMaybe (Err "Da war kein Satz")
            , Array.get 2 array |> Result.fromMaybe (Err "Da war kein Satz")
            ]
    in
    List.any
        (\result ->
            case result of
                Err _ ->
                    True

                Ok _ ->
                    False
        )
        saetzeRichtig
        |> not


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSatzEingabe satzNr eingabe ->
            { model
                | spiel =
                    let
                        resultEingabe =
                            case String.toInt eingabe of
                                Nothing ->
                                    wandleEingabeUm eingabe

                                Just punkte ->
                                    Ok punkte
                    in
                    Array.set
                        satzNr
                        (case resultEingabe of
                            Err mitFehler ->
                                case mitFehler of
                                    "Spieler Eins hat zu Null verloren" ->
                                        Ok (verlorenMit 0)

                                    _ ->
                                        Err mitFehler

                            Ok punkte ->
                                if punkte < 0 then
                                    Ok (verlorenMit punkte)

                                else
                                    Ok (gewonnenMit punkte)
                        )
                        model.spiel
            }

        ErgebnisseUebertragen ->
            let
                spielerEins =
                    case model.gameToUpdate of
                        MitErgebnis _ spiel _ ->
                            spiel.spielerEins

                        OhneErgebnis _ spiel ->
                            spiel.spielerEins

                spielerZwei =
                    case model.gameToUpdate of
                        MitErgebnis _ spiel _ ->
                            spiel.spielerZwei

                        OhneErgebnis _ spiel ->
                            spiel.spielerZwei

                thisErgebnis =
                    ermittleErgebnis (model.spiel |> Array.toList) spielerEins spielerZwei
            in
            { model
                | ergebnis = Just (ermittleErgebnis (model.spiel |> Array.toList) spielerEins spielerZwei)
                , endstand = MitErgebnis model.spielNr (Spiel spielerEins spielerZwei) thisErgebnis
            }

        Close ->
            model


ermittleErgebnis : List (Result String Satz) -> Spieler -> Spieler -> Ergebnis
ermittleErgebnis spiel spielerEins spielerZwei =
    let
        gewinnerDerSaetze =
            spiel
                |> gewonnenHat

        gewonneneSaetzeSpielerEins =
            List.filter (\maybe -> maybe == Just "Eins") gewinnerDerSaetze

        ( gewinner, verlierer ) =
            if List.length gewonneneSaetzeSpielerEins == 3 then
                ( spielerEins, spielerZwei )

            else
                ( spielerZwei, spielerEins )
    in
    Ergebnis
        gewinner
        verlierer
        (List.map transformSaetze spiel |> List.filter (\satz -> satz /= ( 0, 0 )))
        (List.foldr countSaetze 0 spiel)


countSaetze : Result String Satz -> Int -> Int
countSaetze result int =
    case result of
        Err _ ->
            int

        Ok _ ->
            int + 1


transformSaetze : Result String Satz -> Satz
transformSaetze result =
    case result of
        Err _ ->
            ( 0, 0 )

        Ok satz ->
            satz


gewonnenHat : List (Result String Satz) -> List (Maybe String)
gewonnenHat list =
    List.map
        (\result ->
            case result of
                Err _ ->
                    Nothing

                Ok ( uno, duo ) ->
                    if uno > duo then
                        Just "Eins"

                    else
                        Just "Zwei"
        )
        list


pruefeAufErgebnisLogik : List (Maybe String) -> Bool
pruefeAufErgebnisLogik spiel =
    let
        gewonneneSaetzeSpielerEins =
            List.filter (\maybe -> maybe == Just "Eins") spiel

        gewonneneSaetzeSpielerZwei =
            List.filter (\maybe -> maybe == Just "Zwei") spiel
    in
    List.length gewonneneSaetzeSpielerEins == 3 || List.length gewonneneSaetzeSpielerZwei == 3


istValide : Result String Satz -> Bool
istValide result =
    case result of
        Ok ( punkteEins, punkteZwei ) ->
            if punkteEins > 10 then
                if punkteZwei < 10 then
                    True

                else
                    abstandKorrekt punkteEins punkteZwei

            else if punkteZwei > 10 then
                if punkteEins < 10 then
                    True

                else
                    abstandKorrekt punkteEins punkteZwei

            else
                False

        Err "Satz noch nicht eingetragen" ->
            True

        Err _ ->
            False


abstandKorrekt : Int -> Int -> Bool
abstandKorrekt eins zwei =
    eins - zwei == 2 || zwei - eins == 2


verlorenMit : Int -> ( Int, Int )
verlorenMit punkte =
    let
        realPoints =
            negate punkte
    in
    if realPoints < 10 then
        ( realPoints, 11 )

    else
        ( realPoints, realPoints + 2 )


gewonnenMit : Int -> ( Int, Int )
gewonnenMit punkte =
    if punkte < 10 then
        ( 11, punkte )

    else
        ( punkte + 2, punkte )


wandleEingabeUm : String -> Result String Int
wandleEingabeUm ergebnis =
    String.split ":" ergebnis
        |> (\list ->
                case list of
                    first :: second :: [] ->
                        let
                            firstInt =
                                String.toInt first

                            secondInt =
                                String.toInt second
                        in
                        case ( firstInt, secondInt ) of
                            ( Just punkteFirst, Just punkteSecond ) ->
                                if punkteFirst /= 0 then
                                    if punkteFirst > punkteSecond then
                                        Ok punkteSecond

                                    else
                                        Ok (negate punkteFirst)

                                else
                                    Err "Spieler Eins hat zu Null verloren"

                            _ ->
                                Err ("Die Eingabe " ++ ergebnis ++ " ist Fehlerhaft")

                    _ ->
                        Err ("Die Eingabe " ++ ergebnis ++ " ist Fehlerhaft")
           )
