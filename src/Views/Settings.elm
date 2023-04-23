module Views.Settings exposing (settingsView)

import Html exposing (Html, a, div, em, h1, h2, input, label, li, p, pre, section, sup, text, ul)
import Html.Attributes exposing (class, href, step, type_, value)
import Html.Events exposing (onInput)
import List exposing (map)
import Msg exposing (Msg(..))
import String exposing (fromFloat, fromInt)
import Utils exposing (appName, formatNumber, repoUrl)


settingsView : String -> Int -> Int -> Int -> Int -> Float -> Html Msg
settingsView version excerptCount bookCount authorCount tagCount semanticThreshold =
    div
        [ class "settings" ]
        [ h1 [] [ text "Settings ", em [] [ text "&c." ] ]
        , section []
            [ div
                []
                [ h2 [] [ text "Semantic search threshold" ]
                , div
                    []
                    [ label
                        []
                        [ input
                            [ type_ "range"
                            , Html.Attributes.min "0.10"
                            , Html.Attributes.max "0.95"
                            , step "0.01"
                            , value <| fromFloat semanticThreshold
                            , onInput SetSemanticThreshold
                            ]
                            []
                        , text <| fromInt (floor (semanticThreshold * 100))
                        , sup [] [ text "%" ]
                        ]
                    ]
                , p [] [ text "Lower values yield more semantic matches." ]
                , h2 [] [ text "Import/export excerpts" ]
                , p
                    []
                    [ text "Visit the "
                    , a [ href "/import" ] [ text "import page" ]
                    , text "."
                    ]
                , h2 [] [ text "Monk-Mode" ]
                , p []
                    [ a [ href "/monk-mode" ] [ em [] [ text "Monk-Mode" ] ]
                    , text <|
                        " is a forthcoming set of features to further enhance your "
                            ++ appName
                            ++ " experience. Consider joining the waitlist."
                    ]
                ]
            , div
                []
                [ h2 [] [ text "Statistics" ]
                , ul []
                    (map
                        (\( name, n ) ->
                            li
                                []
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
                        [ ( "excerpt", excerptCount )
                        , ( "title", bookCount )
                        , ( "author", authorCount )
                        , ( "tag", tagCount )
                        ]
                    )
                , h2 [] [ text "Version" ]
                , p [] [ pre [] [ text version ] ]
                , h2 [] [ text "Colophon" ]
                , p [ class "colophon" ]
                    [ text <| appName ++ " is an open-source wisdom-indexer created by "
                    , a [ href "https://oxism.com" ] [ text "Dan Motzenbecker" ]
                    , text "."
                    ]
                , p [ class "colophon" ]
                    [ text "Itʼs written in "
                    , a [ href "https://elm-lang.org" ] [ text "Elm" ]
                    , text " and typeset in "
                    , a
                        [ href "https://en.wikipedia.org/wiki/EB_Garamond" ]
                        [ text "EB Garamond" ]
                    , text ". Read the "
                    , a
                        [ href repoUrl ]
                        [ text "source code here" ]
                    , text "."
                    ]
                ]
            ]
        ]
