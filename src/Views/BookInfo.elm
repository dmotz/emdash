module Views.BookInfo exposing (bookInfo)

import Dict exposing (get)
import Html
    exposing
        ( Html
        , a
        , br
        , div
        , h5
        , li
        , section
        , text
        , ul
        )
import Html.Attributes exposing (class, href)
import List exposing (map, repeat)
import Model exposing (Book, BookMap, NeighborMap, Tag)
import Msg exposing (Msg)
import Router exposing (titleToRoute)
import Views.TagSection exposing (tagSection)


bookInfo :
    Book
    -> BookMap
    -> List Tag
    -> Maybe Tag
    -> NeighborMap
    -> Html Msg
bookInfo book books tags pendingTag bookNeighborMap =
    section
        [ class "bookMeta" ]
        [ tagSection
            book.tags
            tags
            pendingTag
        , div
            []
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
                                                    titleToRoute neighbor.title
                                                ]
                                                [ text neighbor.title ]
                                            ]

                                    _ ->
                                        text ""
                            )
                            ids

                    _ ->
                        li
                            [ class "wait" ]
                            [ text "…" ]
                            :: repeat 4 (li [] [ br [] [] ])
                )
            ]
        ]
