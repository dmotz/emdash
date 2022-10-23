module Views.BookInfo exposing (bookInfo)

import Dict exposing (get)
import Html
    exposing
        ( Html
        , a
        , br
        , div
        , h1
        , h2
        , h5
        , input
        , li
        , p
        , section
        , text
        , ul
        )
import Html.Attributes as H
    exposing
        ( class
        , classList
        , href
        , step
        , type_
        , value
        )
import Html.Events exposing (onInput)
import List exposing (map, repeat)
import Maybe exposing (withDefault)
import Model exposing (Book, BookMap, NeighborMap, Tag)
import Msg exposing (Msg(..))
import Router exposing (authorToRoute, titleToRoute)
import String exposing (fromInt, toInt)
import Utils exposing (excerptCountLabel, null)
import Views.TagSection exposing (tagSection)


bookInfo :
    Book
    -> BookMap
    -> List Tag
    -> Maybe Tag
    -> NeighborMap
    -> Html Msg
bookInfo book books tags pendingTag bookNeighborMap =
    div
        [ class "bookInfo" ]
        [ h1 [] [ text book.title ]
        , h2
            []
            [ a [ href <| authorToRoute book.author ] [ text book.author ]
            , text <| " — " ++ excerptCountLabel book.count
            ]
        , section
            [ class "bookMeta" ]
            [ div
                [ class "col" ]
                [ h5 [] [ text "Related" ]
                , ul
                    [ class "related" ]
                    (case get book.id bookNeighborMap of
                        Just ids ->
                            map
                                (\( id, _ ) ->
                                    case
                                        get id books
                                    of
                                        Just neighbor ->
                                            li []
                                                [ a
                                                    [ class "title"
                                                    , href <|
                                                        titleToRoute
                                                            neighbor.title
                                                    ]
                                                    [ text neighbor.title ]
                                                ]

                                        _ ->
                                            null
                                )
                                ids

                        _ ->
                            li
                                [ class "wait" ]
                                [ text "…" ]
                                :: repeat 4 (li [] [ br [] [] ])
                    )
                ]
            , div
                [ class "col" ]
                [ tagSection
                    book.tags
                    tags
                    pendingTag
                , div
                    []
                    [ h5 [] [ text "Rating" ]
                    , div
                        [ class "rating" ]
                        [ p
                            [ classList [ ( "unrated", book.rating == 0 ) ] ]
                            [ text <|
                                if book.rating == 0 then
                                    "—"

                                else
                                    (book.rating
                                        |> toFloat
                                        |> (\n -> n / 2)
                                        |> String.fromFloat
                                    )
                                        ++ "/5"
                            ]
                        , input
                            [ type_ "range"
                            , H.min "0"
                            , H.max "10"
                            , step "2"
                            , value <| fromInt book.rating
                            , onInput <|
                                toInt
                                    >> withDefault 0
                                    >> SetRating book
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]
