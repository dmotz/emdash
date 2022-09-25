module Views.Landing exposing (landingView)

import Html exposing (Html, a, br, button, div, em, footer, h1, h2, h3, h4, img, li, p, span, text, ul)
import Html.Attributes exposing (class, draggable, href, src, target)
import Msg exposing (Msg)


landingView : Html Msg
landingView =
    div [ class "landing" ]
        [ h1 []
            [ img [ src "/logo.svg", draggable "false" ] []
            , text "arginalia organizes highlights from ebooks so you can "
            , em [] [ text "actually remember" ]
            , text " what you read."
            ]
        , footer [] [ text "❦" ]
        ]
