module Views.Excerpt exposing (excerptView)

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
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy4)
import List exposing (isEmpty, map)
import Model exposing (BookMap, Excerpt, ExcerptMap, ExcerptTab(..), ScorePairs)
import Msg exposing (Msg(..))
import Router exposing (excerptToRoute)
import String exposing (fromInt)
import Utils exposing (getExcerptDomId, null)
import Views.Citation exposing (citation)
import Views.Snippet exposing (snippetView)


excerptView :
    ExcerptMap
    -> BookMap
    -> ScorePairs
    -> Bool
    -> ExcerptTab
    -> Int
    -> Bool
    -> Bool
    -> Maybe (Html Msg)
    -> Excerpt
    -> Html Msg
excerptView excerpts books neighbors showDetails activeTab i perma isMarked mProgress excerpt =
    li
        [ classList [ ( "excerpt", True ), ( "permalink", perma ) ]
        , id <| getExcerptDomId excerpt.id
        ]
        [ figure
            []
            [ figcaption
                [ class "meta" ]
                [ if perma then
                    null

                  else
                    div [] [ text <| fromInt (i + 1) ]
                , button
                    [ onClick (ToggleFavorite excerpt.id)
                    , classList
                        [ ( "favorite", True )
                        , ( "active", excerpt.isFavorite )
                        ]
                    ]
                    [ img
                        [ class "favoriteIcon"
                        , src <|
                            "/images/favorite"
                                ++ (if excerpt.isFavorite then
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
                            (if excerpt.isFavorite then
                                "Unmark"

                             else
                                "Mark"
                            )
                                ++ " as favorite"
                        ]
                    ]
                , if perma then
                    null

                  else
                    button
                        [ classList
                            [ ( "bookmark", True )
                            , ( "active", isMarked )
                            ]
                        , onClick (SetBookmark excerpt.bookId excerpt.id)
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
                , if perma then
                    null

                  else
                    a
                        [ href <| excerptToRoute books excerpt ]
                        [ text <|
                            if excerpt.page == -1 then
                                "¶"

                            else
                                "p. " ++ fromInt excerpt.page
                        , div [ class "hint left" ] [ text "Permalink" ]
                        ]
                ]
            , blockquote [] [ text excerpt.text ]
            , if perma then
                case get excerpt.bookId books of
                    Just book ->
                        citation excerpt book Nothing

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
                                SetExcerptTab
                                    excerpt.id
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
                                            ++ (if String.isEmpty excerpt.notes then
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
                                [ class "relatedExcerpts" ]
                                [ case mProgress of
                                    Just progressView ->
                                        progressView

                                    _ ->
                                        if isEmpty neighbors then
                                            p
                                                [ class "wait" ]
                                                [ text "Finding related excerpts…" ]

                                        else
                                            ul [ class "neighbors" ]
                                                (map
                                                    (\( id, score ) ->
                                                        case get id excerpts of
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
                                    [ onInput (UpdateNotes excerpt.id)
                                    , value excerpt.notes
                                    , placeholder "Add notes here"
                                    ]
                                    [ text excerpt.notes ]
                                ]

                        Etc ->
                            section
                                []
                                [ button
                                    [ class "button"
                                    , onClick (HideExcerpt excerpt)
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
