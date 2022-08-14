module Views.Header exposing (headerView)

import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h1
        , header
        , img
        , input
        , label
        , option
        , select
        , span
        , text
        )
import Html.Attributes
    exposing
        ( class
        , classList
        , draggable
        , href
        , placeholder
        , spellcheck
        , src
        , title
        , value
        )
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Model exposing (BookSort(..), Filter(..), InputFocus(..))
import Msg exposing (Msg(..))
import Router exposing (authorToRoute)


headerView : Maybe Filter -> String -> Bool -> Html Msg
headerView filter searchQuery hideHeader =
    header
        [ classList [ ( "hidden", hideHeader ) ] ]
        [ a
            [ class "logo", href "/" ]
            [ img
                [ src "/logo.svg"
                , draggable "false"
                , onClick ToggleAboutMode
                ]
                []
            ]
        , case filter of
            Just (TitleFilter book) ->
                h1 []
                    [ span [ class "title" ] [ text book.title ]
                    , span [ class "divider" ] [ text "|" ]
                    , a
                        [ href <| authorToRoute book.author ]
                        [ text book.author ]
                    ]

            Just (AuthorFilter author) ->
                h1 [] [ text author ]

            _ ->
                div
                    [ class "search" ]
                    [ input
                        [ onInput OnSearch
                        , onFocus <| SetInputFocus (Just SearchFocus)
                        , onBlur <| SetInputFocus Nothing
                        , spellcheck False
                        , placeholder "search"
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
        ]
