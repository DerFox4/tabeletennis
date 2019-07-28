module Main exposing (main)

import Browser
import FunctionHelper.SpielerEingabe as SpielerEingabe exposing (Msg(..))
import GameModule.AlleGegenAlle as AlleGegenAlle exposing (Msg(..))
import GameModule.EinfachesKO as EinfachesKO exposing (Msg(..))
import Html exposing (Html, button, div, h1, hr, node, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types.ProjectTyps as ProjectTyps exposing (Spieler)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = WaehleTurnierModus TurnierModus
    | UpdateAlleGegenAlle AlleGegenAlle.Model AlleGegenAlle.Msg
    | UpdateSpielerEingabe SpielerEingabe.Model SpielerEingabe.Msg
    | UpdateEinfachesKO EinfachesKO.Model EinfachesKO.Msg
    | EingabeStarten


type AktivesFenster
    = SpielerEingabe SpielerEingabe.Model
    | ModusAuswahl


type TurnierModus
    = All AlleGegenAlle.Model
    | KO EinfachesKO.Model


type alias Model =
    { aktivesFenster : Maybe AktivesFenster
    , turniermodus : Maybe TurnierModus
    , spielerListe : List Spieler
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { aktivesFenster = Nothing
      , turniermodus = Nothing
      , spielerListe = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WaehleTurnierModus modus ->
            ( { model | turniermodus = Just modus }, Cmd.none )

        UpdateAlleGegenAlle alleGegenAlleModel alleGegenAlleMsg ->
            ( { model | turniermodus = Just (All (AlleGegenAlle.update alleGegenAlleMsg alleGegenAlleModel)) }, Cmd.none )

        UpdateEinfachesKO einfachesKOModel einfachesKOMsg ->
            ( { model | turniermodus = Just (KO (EinfachesKO.update einfachesKOMsg einfachesKOModel)) }, Cmd.none )

        UpdateSpielerEingabe spielerEingabeModel spielerEingabeMsg ->
            case spielerEingabeMsg of
                StarteTurnier ->
                    ( { model
                        | spielerListe = spielerEingabeModel.listSpieler
                        , aktivesFenster = Just ModusAuswahl
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | aktivesFenster = Just (SpielerEingabe (SpielerEingabe.update spielerEingabeMsg spielerEingabeModel)) }, Cmd.none )

        EingabeStarten ->
            ( { model | aktivesFenster = Just (SpielerEingabe SpielerEingabe.init) }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ node "link"
            [ Html.Attributes.rel "stylesheet", Html.Attributes.href "main.css" ]
            []
        , h1 [] [ text "Turnierorganisator 1.0" ]
        , hr [] []
        , div [ class "View" ]
            [ case model.aktivesFenster of
                Nothing ->
                    div [ class "Start" ]
                        [ button [ onClick EingabeStarten ] [ text "Starten Sie nun mit der Eingabe der Teilnehmer des Turniers:" ]
                        ]

                Just (SpielerEingabe modelSpielerEingabe) ->
                    div []
                        [ Html.map (updateSpielerEingabe modelSpielerEingabe) (SpielerEingabe.view modelSpielerEingabe) ]

                Just ModusAuswahl ->
                    case model.turniermodus of
                        Nothing ->
                            div []
                                [ div [] [ text "Nun können Sie hier zwischen mehreren Turnierformen wählen, um dann ein Turnier nachdem entsprechendem System zu erstellen." ]
                                , div [] [ text "Der 'Turnierorganisator' übernimmt für Sie die Erstellung der Paarungen." ]
                                , div [] [ text "Dies geschiet je nach Turnierform völlig unterschiedlich." ]
                                , hr [] []
                                , div []
                                    [ button [ onClick (WaehleTurnierModus (All (AlleGegenAlle.init model.spielerListe))) ] [ text "Alle Gegen Alle" ]
                                    , button [ onClick (WaehleTurnierModus (KO (EinfachesKO.init model.spielerListe))) ] [ text "EinfachesKO" ]
                                    ]
                                ]

                        Just (All alleGegenAlleModel) ->
                            div []
                                [ Html.map (updateAlleGegenAlle alleGegenAlleModel) (AlleGegenAlle.view alleGegenAlleModel) ]

                        Just (KO einfachesKOModel) ->
                            div []
                                [ Html.map (updateEinfachesKO einfachesKOModel) (EinfachesKO.view einfachesKOModel) ]
            ]
        ]


updateSpielerEingabe : SpielerEingabe.Model -> SpielerEingabe.Msg -> Msg
updateSpielerEingabe model msg =
    UpdateSpielerEingabe model msg


updateAlleGegenAlle : AlleGegenAlle.Model -> AlleGegenAlle.Msg -> Msg
updateAlleGegenAlle model msg =
    UpdateAlleGegenAlle model msg


updateEinfachesKO : EinfachesKO.Model -> EinfachesKO.Msg -> Msg
updateEinfachesKO model msg =
    UpdateEinfachesKO model msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
