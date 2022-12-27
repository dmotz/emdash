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
import List exposing (isEmpty, map)
import Model
    exposing
        ( BookMap
        , Entry
        , EntryMap
        , EntryTab(..)
        , InputFocus(..)
        , ScorePairs
        )
import Msg exposing (Msg(..))
import Router exposing (entryToRoute)
import String exposing (fromInt)
import Utils exposing (getEntryDomId, null)
import Views.Citation exposing (citation)
import Views.Snippet exposing (snippetView)


entryView :
    EntryMap
    -> BookMap
    -> ScorePairs
    -> Bool
    -> EntryTab
    -> Int
    -> Bool
    -> Bool
    -> Maybe (Html Msg)
    -> Entry
    -> Html Msg
entryView entries books neighbors showDetails activeTab i perma isMarked mProgress entry =
    li
        [ classList [ ( "entry", True ), ( "permalink", perma ) ]
        , id <| getEntryDomId entry.id
        ]
        [ figure []
            [ if not perma then
                figcaption
                    [ class "meta" ]
                    [ div [] [ text <| fromInt (i + 1) ]
                    , button
                        [ onClick (ToggleFavorite entry.id)
                        , classList
                            [ ( "favorite", True )
                            , ( "active", entry.isFavorite )
                            ]
                        ]
                        [ img
                            [ class "favoriteIcon"
                            , src <|
                                "/images/favorite"
                                    ++ (if entry.isFavorite then
                                            "-filled"

                                        else
                                            ""
                                       )
                                    ++ ".svg"
                            ]
                            []
                        , div
                            [ class "hint left" ]
                            [ text <|
                                (if entry.isFavorite then
                                    "Unmark"

                                 else
                                    "Mark"
                                )
                                    ++ " as favorite"
                            ]
                        ]
                    , if perma then
                        text ""

                      else
                        button
                            [ classList
                                [ ( "bookmark", True )
                                , ( "active", isMarked )
                                ]
                            , onClick (SetBookmark entry.bookId entry.id)
                            ]
                            [ img
                                [ src <|
                                    "/images/bookmark"
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
                                [ text <|
                                    (if isMarked then
                                        "Unmark"

                                     else
                                        "Mark"
                                    )
                                        ++ " as last read in this book"
                                ]
                            ]
                    , a
                        [ href <| entryToRoute books entry ]
                        [ text <|
                            if entry.page == -1 then
                                "¶"

                            else
                                "p. " ++ fromInt entry.page
                        , div [ class "hint left" ] [ text "Permalink" ]
                        ]
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
            , div
                [ classList [ ( "tabs", True ), ( "active", showDetails ) ] ]
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
                                            ++ (if String.isEmpty entry.notes then
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
                div
                    [ class "details" ]
                    [ case activeTab of
                        Related ->
                            section
                                [ class "related" ]
                                [ case mProgress of
                                    Just progressView ->
                                        progressView

                                    _ ->
                                        if isEmpty neighbors then
                                            p
                                                [ class "wait" ]
                                                [ text "Finding related entries…" ]

                                        else
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
                                ]

                        Notes ->
                            section
                                [ class "notes" ]
                                [ textarea
                                    [ onFocus <| SetInputFocus (Just NoteFocus)
                                    , onInput (UpdateNotes entry.id)
                                    , value entry.notes
                                    , placeholder "Add notes here"
                                    ]
                                    [ text entry.notes ]
                                ]

                        Etc ->
                            section
                                []
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
