module Views.About exposing (about)

import Html exposing (Html, a, div, footer, h4, li, ol, p, span, text)
import Html.Attributes exposing (class, href, id, target)
import Html.Events exposing (onClick)
import List exposing (length, map)
import Model exposing (Author, Entry, Tag, Title)
import Msg exposing (Msg(..))
import Utils exposing (formatNumber)
import Views.Common exposing (repoUrl)


about : List Entry -> List Title -> List Author -> List Tag -> Html Msg
about entries titles authors tags =
    div [ id "about" ]
        [ div [ class "info-page" ]
            [ div
                [ class "hide-button", onClick ToggleAboutMode ]
                [ div [] [ text "×" ] ]
            , p []
                [ text
                    "Marginalia is an open source tool created by "
                , a [ href "https://oxism.com", target "_blank" ]
                    [ text "Dan Motzenbecker" ]
                , text "."
                ]
            , div
                [ class "col-2" ]
                [ div []
                    [ h4 [] [ text "Actions" ]
                    , p []
                        [ a [ onClick ExportJson ]
                            [ text "Export "
                            , span [ class "small-caps" ] [ text "json" ]
                            ]
                        ]
                    , p []
                        [ a [ onClick ImportJson ]
                            [ text "Import "
                            , span [ class "small-caps" ] [ text "json" ]
                            ]
                        ]
                    , p []
                        [ a [ onClick ExportEpub ]
                            [ text "Export "
                            , span [ class "small-caps" ] [ text "epub" ]
                            ]
                        ]
                    , p []
                        [ a
                            [ href repoUrl
                            , target "_blank"
                            ]
                            [ text "Read the source" ]
                        ]
                    ]
                , div []
                    [ h4 [] [ text "Statistics" ]
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
                            [ ( "excerpt", length entries )
                            , ( "title", length titles )
                            , ( "author", length authors )
                            , ( "tag", length tags )
                            ]
                        )
                    ]
                ]
            , h4 [] [ text "Colophon" ]
            , p []
                [ text "Marginalia is written in "
                , a
                    [ href "https://elm-lang.org/", target "_blank" ]
                    [ text "Elm" ]
                , text " and typeset in "
                , a
                    [ href "https://github.com/impallari/Libre-Baskerville"
                    , target "_blank"
                    ]
                    [ text "Libre Baskerville" ]
                , text "."
                ]
            , p [] [ text "❦" ]
            , footer [] [ text "Habent sua fata libelli" ]
            ]
        ]
