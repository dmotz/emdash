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
        , target
        , type_
        , value
        )
import Html.Events exposing (onInput)
import List exposing (map, repeat)
import Maybe exposing (withDefault)
import Model exposing (Book, BookMap, Id, NeighborMap, Tag)
import Msg exposing (Msg(..))
import Router exposing (authorToRoute, titleSlugToRoute)
import String exposing (fromInt)
import Utils exposing (excerptCountLabel, getEntryDomId, null, ratingEl)
import Views.TagSection exposing (tagSection)


bookInfo :
    Book
    -> BookMap
    -> List Tag
    -> Maybe Tag
    -> NeighborMap
    -> Maybe Id
    -> Maybe (Html Msg)
    -> Html Msg
bookInfo book books tags pendingTag bookNeighborMap mLastRead progressView =
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
                , case progressView of
                    Just view ->
                        view

                    _ ->
                        ul
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
                                                                titleSlugToRoute
                                                                    neighbor.slug
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
                        [ ratingEl book
                        , input
                            [ type_ "range"
                            , H.min "0"
                            , H.max "5"
                            , step "0.5"
                            , value <| String.fromFloat book.rating
                            , onInput <|
                                String.toFloat
                                    >> withDefault 0
                                    >> SetRating book
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , case mLastRead of
            Just id ->
                a
                    [ href <| "#" ++ getEntryDomId id, target "_self" ]
                    [ text "↧ Jump to last read excerpt" ]

            _ ->
                a [ href "#", class "hidden" ] [ text "↧" ]
        ]
