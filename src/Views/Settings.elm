module Views.Settings exposing (settingsView)

import Html exposing (Html, a, button, div, em, h1, h2, li, ol, p, section, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import List exposing (map)
import Msg exposing (Msg(..))
import Utils exposing (formatNumber)


settingsView : Int -> Int -> Int -> Int -> Html Msg
settingsView entryCount bookCount authorCount tagCount =
    div
        [ class "settings" ]
        [ h1 [] [ text "Settings ", em [] [ text "&c." ] ]
        , section []
            [ div
                []
                [ h2 [] [ text "Statistics" ]
                , ol []
                    (map
                        (\( name, n ) ->
                            li []
                                [ text <|
                                    formatNumber n
                                        ++ " "
                                        ++ name
                                        ++ (if n /= 1 then
                                                "s"

                                            else
                                                ""
                                           )
                                ]
                        )
                        [ ( "excerpt", entryCount )
                        , ( "title", bookCount )
                        , ( "author", authorCount )
                        , ( "tag", tagCount )
                        ]
                    )
                , h2 [] [ text "Colophon" ]
                , p [ class "colophon" ]
                    [ text "Marginalia is an open-source tool created by "
                    , a [ href "https://oxism.com" ] [ text "Dan Motzenbecker" ]
                    , text "."
                    ]
                , p [ class "colophon" ]
                    [ text "Itʼs written in "
                    , a [ href "https://elm-lang.org/" ] [ text "Elm" ]
                    , text " and typeset in "
                    , a
                        [ href "https://en.wikipedia.org/wiki/EB_Garamond" ]
                        [ text "EB Garamond" ]
                    , text ". Read the "
                    , a
                        [ href "https://github.com/dmotz/marginalia" ]
                        [ text "source code here" ]
                    , text "."
                    ]
                ]
            , div
                []
                [ h2 [] [ text "Imports ", em [] [ text "&" ], text " exports" ]
                , button
                    [ class "button"
                    , onClick ExportJson
                    ]
                    [ text "Export "
                    , span
                        [ class "smallCaps" ]
                        [ text "json" ]
                    ]
                , p
                    []
                    [ text "Exports your full collection including tags and notes for safekeeping." ]
                , button
                    [ class "button"
                    , onClick ExportEpub
                    ]
                    [ text "Export "
                    , span
                        [ class "smallCaps" ]
                        [ text "epub" ]
                    ]
                , p []
                    [ text "Exports your excerpts into an "
                    , span [ class "smallCaps" ] [ text "epub" ]
                    , text " file for review on an e-reader."
                    ]
                , button
                    [ class "button"
                    , onClick ExportEpub
                    ]
                    [ text "Import "
                    , span
                        [ class "smallCaps" ]
                        [ text "json" ]
                    ]
                , p []
                    [ text "Restore your collection via a previously exported "
                    , span [ class "smallCaps" ] [ text "json" ]
                    , text " file."
                    ]
                ]
            ]
        ]
