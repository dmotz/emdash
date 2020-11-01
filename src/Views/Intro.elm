module Views.Intro exposing (intro)

import Html exposing (Html, a, div, em, footer, h4, li, ol, p, span, text)
import Html.Attributes exposing (class, href, id, target)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Views.Common exposing (repoUrl)


intro : Html Msg
intro =
    div []
        [ p [ class "big" ]
            [ text <|
                "This is Marginalia, a tool to organize excerpts from ebooks "
                    ++ "with tags, notes, and search."
            ]
        , h4 [] [ text "To begin:" ]
        , div [ id "instructions" ]
            [ p []
                [ text <|
                    "Drop a clippings text file onto this page to import "
                        ++ "its excerpts."
                ]
            , p []
                [ text "Or, "
                , a [ onClick PickFile ] [ text "click here" ]
                , text " to browse for the file."
                ]
            ]
        , h4 [] [ em [] [ text "Nota bene:" ] ]
        , ol []
            [ li []
                [ text <|
                    "Marginalia works entirely on your device and stores all "
                        ++ "your data there."
                ]
            , li []
                [ text <|
                    "You can easily export your data (tags, notes, &c.) as "
                , span [ class "small-caps" ] [ text "json" ]
                , text " or "
                , span [ class "small-caps" ] [ text "epub" ]
                , text "."
                ]
            , li []
                [ text "It works offline." ]
            , li []
                [ text "It’s "
                , a [ href repoUrl, target "_blank" ] [ text "open source" ]
                , text "."
                ]
            , li [] [ text "You might like it." ]
            ]
        , p [] [ text "❦" ]
        , footer [] [ text "Habent sua fata libelli" ]
        ]
