module Views.Button exposing (actionButton)

import Html exposing (button, div)
import Html.Attributes exposing (class)


actionButton : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
actionButton attrs children =
    button
        [ class "actionButton" ]
        [ div (class "buttonContent" :: attrs) children
        , div [ class "buttonShadow" ] []
        ]
