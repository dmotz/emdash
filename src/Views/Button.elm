module Views.Button exposing (actionButton)

import Html exposing (button, div)
import Html.Attributes exposing (class)


actionButton : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
actionButton attrs children =
    button
        (class "actionButton" :: attrs)
        [ div [ class "buttonContent" ] children
        , div [ class "buttonShadow" ] []
        ]
