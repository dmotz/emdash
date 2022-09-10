module Views.Entry exposing (entryView)

import Dict exposing (get)
import Html
    exposing
        ( Html
        , blockquote
        , button
        , div
        , figcaption
        , figure
        , li
        , p
        , section
        , span
        , text
        , textarea
        , ul
        )
import Html.Attributes exposing (class, classList, id, placeholder, value)
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
import String exposing (fromInt, isEmpty)
import Utils exposing (getEntryDomId)
import Views.Snippet exposing (snippetView)


entryView :
    EntryMap
    -> BookMap
    -> NeighborMap
    -> Bool
    -> EntryTab
    -> Int
    -> Entry
    -> Html Msg
entryView entries books neighborMap showDetails activeTab i entry =
    li
        [ class "entry", getEntryDomId entry.id |> id ]
        [ figure []
            [ figcaption [ class "meta" ]
                [ div [] [ text <| fromInt (i + 1) ++ "." ]
                , if entry.page /= -1 then
                    div [ class "page" ] [ text <| "p. " ++ fromInt entry.page ]

                  else
                    text ""
                ]
            , blockquote [] [ text entry.text ]
            , div
                [ classList
                    [ ( "detailsBar", True )
                    , ( "active", showDetails )
                    ]
                ]
                [ button
                    [ class "detailsToggle"
                    , onClick (ToggleDetails entry.id)
                    ]
                    [ span []
                        [ text
                            (if showDetails then
                                "▼"

                             else
                                "▶"
                            )
                        ]
                    , span [] [ text "‡" ]
                    , if showDetails then
                        text ""

                      else
                        div
                            [ class "hint" ]
                            [ span [] [ text "☜" ]
                            , text "show related excerpts, notes, &c"
                            ]
                    ]
                , if showDetails then
                    div [ class "tabs" ]
                        (map
                            (\tab ->
                                button
                                    [ onClick <| SetEntryTab entry.id tab
                                    , classList
                                        [ ( "active", tab == activeTab )
                                        ]
                                    ]
                                    [ text <|
                                        case tab of
                                            Related ->
                                                "Related"

                                            Notes ->
                                                "Notes"
                                                    ++ (if
                                                            isEmpty
                                                                entry.notes
                                                        then
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

                  else
                    text ""
                ]
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
                                                            text ""
                                                )
                                                neighbors
                                            )

                                    _ ->
                                        p
                                            [ class "wait" ]
                                            [ text "Calculating related entries…" ]
                                ]

                        Notes ->
                            section [ class "notes" ]
                                [ textarea
                                    [ onFocus <| SetInputFocus (Just NoteFocus)
                                    , onInput (UpdateNotes entry.id)
                                    , value entry.notes
                                    , placeholder "add notes here"
                                    ]
                                    [ text entry.notes ]
                                ]

                        Etc ->
                            section []
                                [ button
                                    [ class "button"
                                    , onClick (HideEntry entry.id)
                                    ]
                                    [ text "× Delete" ]
                                ]
                    ]

              else
                text ""
            ]
        ]
