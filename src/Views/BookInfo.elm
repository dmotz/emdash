module Views.BookInfo exposing (bookInfo)

import Dict exposing (get)
import Html
    exposing
        ( Html
        , a
        , br
        , button
        , div
        , h1
        , h2
        , h5
        , input
        , li
        , section
        , span
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
import Html.Events exposing (onClick, onInput)
import List exposing (intersperse, map, repeat)
import Maybe exposing (withDefault)
import Model exposing (Book, BookMap, ExcerptSort(..), Id, NeighborMap, Tag)
import Msg exposing (Msg(..))
import Router exposing (authorToRoute, titleSlugToRoute)
import Utils exposing (excerptCountLabel, formatScore, getExcerptDomId, null, ratingEl)
import Views.TagSection exposing (tagSection)


bookInfo :
    Book
    -> BookMap
    -> List Tag
    -> Maybe Tag
    -> NeighborMap
    -> Maybe Id
    -> ExcerptSort
    -> Maybe (Html Msg)
    -> Html Msg
bookInfo book books tags pendingTag bookNeighborMap mBookmark excerptSort progressView =
    div
        [ class "bookInfo" ]
        [ h1 [] [ text book.title ]
        , h2
            []
            ((map
                (\author -> a [ href <| authorToRoute author ] [ text author ])
                book.authors
                |> intersperse (text " / ")
             )
                ++ [ text <| " — " ++ excerptCountLabel book.count ]
            )
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
                                        (\( id, score ) ->
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
                                                        , span
                                                            [ class "score" ]
                                                            [ formatScore score
                                                            , div
                                                                [ class "hint" ]
                                                                [ text "Similarity score" ]
                                                            ]
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
        , div
            [ class "actions" ]
            [ div
                [ class "modeHeading" ]
                [ ul
                    []
                    (map
                        (\sort ->
                            li
                                [ classList [ ( "active", sort == excerptSort ) ] ]
                                [ button
                                    [ onClick <| SortExcerpts sort ]
                                    [ span
                                        []
                                        [ text <| sortToString sort
                                        , if
                                            sort
                                                == ExcerptSemanticSort
                                                && excerptSort
                                                /= ExcerptSemanticSort
                                          then
                                            div
                                                [ class "hint" ]
                                                [ text "Sort by most semantically relevant to all passages" ]

                                          else
                                            null
                                        ]
                                    ]
                                ]
                        )
                        [ ExcerptPageSort, ExcerptFavSort, ExcerptSemanticSort ]
                    )
                ]
            , case mBookmark of
                Just id ->
                    a
                        [ href <| "#" ++ getExcerptDomId id, target "_self" ]
                        [ text "↧ Jump to last read excerpt" ]

                _ ->
                    null
            ]
        ]


sortToString : ExcerptSort -> String
sortToString sort =
    case sort of
        ExcerptPageSort ->
            "Page №"

        ExcerptFavSort ->
            "Favorites"

        ExcerptSemanticSort ->
            "Relevance"
