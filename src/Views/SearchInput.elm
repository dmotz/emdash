module Views.SearchInput exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, spellcheck, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Model exposing (InputFocus(..))
import Msg exposing (Msg(..))


searchInput : String -> Html Msg
searchInput searchQuery =
    div
        [ class "search" ]
        [ input
            [ onInput OnSearch
            , onFocus <| SetInputFocus (Just SearchFocus)
            , onBlur <| SetInputFocus Nothing
            , spellcheck False
            , placeholder "𐫱 search"
            , value searchQuery
            ]
            []
        , button
            [ onClick <| OnSearch ""
            , class
                (if String.isEmpty searchQuery then
                    ""

                 else
                    "active"
                )
            ]
            [ text "✕" ]
        ]
