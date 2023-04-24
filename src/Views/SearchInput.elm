module Views.SearchInput exposing (searchInput)

import Html exposing (Html, button, div, input, text)
import Html.Attributes
    exposing
        ( attribute
        , autocomplete
        , class
        , placeholder
        , spellcheck
        , title
        , value
        )
import Html.Events exposing (onClick, onInput)
import Msg exposing (Msg(..))


searchInput : String -> Html Msg
searchInput searchQuery =
    div
        [ class "search" ]
        [ input
            [ onInput OnSearchStart
            , spellcheck False
            , placeholder "Search your library"
            , value searchQuery
            , attribute "enterkeyhint" "search"
            , attribute "autocapitalize" "off"
            , autocomplete False
            ]
            []
        , button
            [ onClick <| OnSearchStart ""
            , class
                (if String.isEmpty searchQuery then
                    ""

                 else
                    "active"
                )
            , title "Clear"
            ]
            [ text "✕" ]
        ]
