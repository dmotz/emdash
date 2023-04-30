module Views.BookInfo exposing (bookInfo)

import Dict exposing (get)
import Html
    exposing
        ( Html
        , a
        , br
        , button
        , details
        , div
        , em
        , form
        , h1
        , h2
        , h5
        , input
        , li
        , section
        , span
        , summary
        , text
        , textarea
        , ul
        )
import Html.Attributes as H
    exposing
        ( class
        , classList
        , href
        , placeholder
        , spellcheck
        , step
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import List exposing (indexedMap, intersperse, map, repeat)
import Maybe exposing (withDefault)
import Msg exposing (Msg(..))
import Router exposing (authorToRoute, titleSlugToRoute)
import String exposing (fromInt, join)
import Types exposing (Book, BookMap, ExcerptSort(..), Id, NeighborMap, Tag)
import Utils
    exposing
        ( excerptCountLabel
        , formatScore
        , getExcerptDomId
        , null
        , ratingEl
        )
import Views.Button exposing (actionButton)
import Views.TagSection exposing (tagSection)


bookInfo :
    Book
    -> BookMap
    -> List Tag
    -> Maybe Tag
    -> NeighborMap
    -> Int
    -> Maybe Id
    -> ExcerptSort
    -> Maybe (Html Msg)
    -> Bool
    -> Html Msg
bookInfo book books tags pendingTag bookNeighborMap count mBookmark excerptSort progressView editMode =
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
                ++ [ text <| " — " ++ excerptCountLabel count ]
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
                        Keyed.ul
                            [ class "related" ]
                            (case get book.id bookNeighborMap of
                                Just ids ->
                                    indexedMap
                                        (\i ( id, score ) ->
                                            ( book.id ++ id
                                            , case
                                                get id books
                                              of
                                                Just neighbor ->
                                                    li
                                                        [ style
                                                            "animation-delay"
                                                            (fromInt (i * 99) ++ "ms")
                                                        ]
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
                                        )
                                        ids

                                _ ->
                                    li
                                        [ class "wait" ]
                                        [ text "…" ]
                                        :: repeat 5 (li [] [ br [] [] ])
                                        |> indexedMap (\i el -> ( fromInt i, el ))
                            )
                ]
            , div
                [ class "col" ]
                [ div
                    [ class "tagsRating" ]
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
                , details []
                    [ summary []
                        [ h5 []
                            [ text "Edit"
                            , if book.notes /= "" then
                                span [] [ em [] [ text " &" ], text " view notes" ]

                              else
                                null
                            ]
                        ]
                    , if editMode then
                        form
                            [ class "editTitle", onSubmit SetBookEdits ]
                            [ input
                                [ value book.title
                                , onInput SetPendingBookTitle
                                , placeholder "Title"
                                , spellcheck False
                                ]
                                []
                            , input
                                [ value <| join "/" book.authors
                                , onInput SetPendingBookAuthor
                                , placeholder "Author"
                                , spellcheck False
                                ]
                                []
                            , div []
                                [ actionButton [] [ text "Save" ]
                                , actionButton
                                    [ onClick ExitBookEditMode ]
                                    [ text "Cancel" ]
                                ]
                            ]

                      else
                        div
                            []
                            [ actionButton
                                [ onClick EnterBookEditMode ]
                                [ text "Edit title / author" ]
                            , actionButton
                                [ onClick <|
                                    ShowConfirmation
                                        "Delete this title and all of its excerpts?"
                                        (DeleteBook book)
                                ]
                                [ text "Delete" ]
                            , textarea
                                [ placeholder "Add notes about this title here. Perhaps your review?"
                                , value book.notes
                                , onInput <| UpdateBookNotes book.id
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
