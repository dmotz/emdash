module Views.Entry exposing (entryView)

import Dict exposing (get)
import Html
    exposing
        ( Html
        , a
        , blockquote
        , button
        , div
        , figcaption
        , figure
        , hr
        , img
        , li
        , p
        , section
        , text
        , textarea
        , ul
        )
import Html.Attributes
    exposing
        ( class
        , classList
        , href
        , id
        , placeholder
        , src
        , value
        )
import Html.Events exposing (onClick, onFocus, onInput)
import Html.Lazy exposing (lazy4)
import List exposing (map)
import Model
    exposing
        ( BookMap
        , Entry
        , EntryMap
        , EntryTab(..)
        , InputFocus(..)
        , NeighborMap
        )
import Msg exposing (Msg(..))
import Router exposing (entryToRoute)
import String exposing (fromInt, isEmpty)
import Utils exposing (getEntryDomId, null)
import Views.Citation exposing (citation)
import Views.Snippet exposing (snippetView)


entryView :
    EntryMap
    -> BookMap
    -> NeighborMap
    -> Bool
    -> EntryTab
    -> Int
    -> Bool
    -> Bool
    -> Entry
    -> Html Msg
entryView entries books neighborMap showDetails activeTab i perma isMarked entry =
    li
        [ classList [ ( "entry", True ), ( "permalink", perma ) ]
        , id <| getEntryDomId entry.id
        ]
        [ figure []
            [ if not perma then
                figcaption [ class "meta" ]
                    [ div [] [ text <| fromInt (i + 1) ]
                    , if perma then
                        text ""

                      else
                        button
                            [ classList
                                [ ( "bookmark", True )
                                , ( "bookmarked", isMarked )
                                ]
                            , onClick (SetLastRead entry.bookId entry.id)
                            ]
                            [ img
                                [ src <|
                                    "/bookmark"
                                        ++ (if isMarked then
                                                "-filled"

                                            else
                                                ""
                                           )
                                        ++ ".svg"
                                ]
                                []
                            , div
                                [ class "hint left" ]
                                [ text "Mark as last read in this book" ]
                            ]
                    , if entry.page /= -1 then
                        div
                            [ class "page" ]
                            [ text <| "p. " ++ fromInt entry.page ]

                      else
                        null
                    , a [ href <| entryToRoute books entry ] [ text "¶" ]
                    ]

              else
                null
            , blockquote [] [ text entry.text ]
            , if perma then
                case get entry.bookId books of
                    Just book ->
                        citation entry book Nothing

                    _ ->
                        null

              else
                null
            , div [ classList [ ( "tabs", True ), ( "active", showDetails ) ] ]
                (map
                    (\tab ->
                        button
                            [ onClick <|
                                SetEntryTab
                                    entry.id
                                    tab
                                    (not perma
                                        && (not showDetails || tab == activeTab)
                                    )
                            , classList
                                [ ( "active"
                                  , showDetails && tab == activeTab
                                  )
                                ]
                            ]
                            [ text <|
                                case tab of
                                    Related ->
                                        "Related"

                                    Notes ->
                                        "Notes"
                                            ++ (if isEmpty entry.notes then
                                                    ""

                                                else
                                                    " °"
                                               )

                                    Etc ->
                                        "&c."
                            ]
                    )
                    [ Related, Notes, Etc ]
                )
            , if showDetails then
                div [ class "details" ]
                    [ case activeTab of
                        Related ->
                            section [ class "related" ]
                                [ case get entry.id neighborMap of
                                    Just neighbors ->
                                        ul [ class "neighbors" ]
                                            (map
                                                (\( id, score ) ->
                                                    case get id entries of
                                                        Just neighbor ->
                                                            lazy4
                                                                snippetView
                                                                books
                                                                (Just score)
                                                                Nothing
                                                                neighbor

                                                        _ ->
                                                            null
                                                )
                                                neighbors
                                            )

                                    _ ->
                                        p
                                            [ class "wait" ]
                                            [ text "Finding related entries…" ]
                                ]

                        Notes ->
                            section [ class "notes" ]
                                [ textarea
                                    [ onFocus <| SetInputFocus (Just NoteFocus)
                                    , onInput (UpdateNotes entry.id)
                                    , value entry.notes
                                    , placeholder "Add notes here"
                                    ]
                                    [ text entry.notes ]
                                ]

                        Etc ->
                            section []
                                [ button
                                    [ class "button"
                                    , onClick (HideEntry entry.id)
                                    ]
                                    [ text "× Delete"
                                    , div
                                        [ class "hint" ]
                                        [ text "Remove this excerpt from your collection" ]
                                    ]
                                ]
                    ]

              else
                null
            , hr [] []
            ]
        ]
