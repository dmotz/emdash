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
import List exposing (map)
import Model
    exposing
        ( BookSort(..)
        , Filter(..)
        , InputFocus(..)
        , sortToString
        , stringToSort
        )
import Msg exposing (Msg(..))
import Router exposing (authorToRoute)


headerView : Maybe Filter -> String -> Bool -> Bool -> Html Msg
headerView filter searchQuery reverseSort hideHeader =
    header
        [ classList [ ( "hidden", hideHeader ) ] ]
        (a [ class "logo", href "/" ]
            [ img
                [ src "/logo.svg"
                , draggable "false"
                , onClick ToggleAboutMode
                ]
                []
            ]
            :: (case filter of
                    Just (TitleFilter book) ->
                        [ h1 []
                            [ span [ class "title" ] [ text book.title ]
                            , span [ class "divider" ] [ text "|" ]
                            , a
                                [ href <| authorToRoute book.author ]
                                [ text book.author ]
                            ]
                        ]

                    _ ->
                        [ div
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
                        , div [ class "sorter" ]
                            [ label []
                                [ text "sort by: "
                                , select [ onInput (stringToSort >> SortBooks) ]
                                    (map
                                        (\sort ->
                                            let
                                                s =
                                                    sortToString sort
                                            in
                                            option [ value s ] [ text s ]
                                        )
                                        [ RecencySort, TitleSort, NumSort ]
                                    )
                                ]
                            , button
                                [ onClick Sort
                                , title <|
                                    "sort "
                                        ++ (if reverseSort then
                                                "descending"

                                            else
                                                "ascending"
                                           )
                                ]
                                [ text <|
                                    if reverseSort then
                                        "▲"

                                    else
                                        "▼"
                                ]
                            ]
                        ]
               )
        )
