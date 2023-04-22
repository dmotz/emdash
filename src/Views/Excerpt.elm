module Views.Excerpt exposing (excerptView)

import Dict exposing (get)
import Html
    exposing
        ( Html
        , a
        , blockquote
        , button
        , details
        , div
        , em
        , figcaption
        , figure
        , hr
        , img
        , li
        , p
        , section
        , span
        , summary
        , text
        , textarea
        , ul
        )
import Html.Attributes
    exposing
        ( class
        , classList
        , draggable
        , href
        , id
        , placeholder
        , src
        , value
        )
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy4)
import List exposing (head, isEmpty, map)
import Maybe exposing (andThen)
import Msg exposing (Msg(..))
import Router exposing (excerptToRoute)
import String exposing (fromInt)
import Types
    exposing
        ( BookMap
        , Excerpt
        , ExcerptMap
        , ExcerptTab(..)
        , Lens(..)
        , ScorePairs
        )
import Utils exposing (getExcerptDomId, lensToString, null)
import Views.Button exposing (actionButton)
import Views.Citation exposing (citation)
import Views.Snippet exposing (snippetView)


excerptView :
    ExcerptMap
    -> BookMap
    -> ScorePairs
    -> Bool
    -> ExcerptTab
    -> Bool
    -> Int
    -> Bool
    -> Bool
    -> Maybe (Html Msg)
    -> Excerpt
    -> Html Msg
excerptView excerpts books neighbors showDetails activeTab showLensTab i perma isMarked mProgress excerpt =
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
                    [ onClick (ToggleFavorite excerpt)
                    , classList
                        [ ( "favorite", True )
                        , ( "active", excerpt.isFavorite )
                        ]
                    ]
                    [ img
                        [ class "icon"
                        , draggable "false"
                        , src <|
                            "/images/icons/favorite"
                                ++ (if excerpt.isFavorite then
                                        "-filled"

                                    else
                                        ""
                                   )
                                ++ ".svg"
                        ]
                        []
                    , div
                        [ class "hint left swapRight" ]
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
                            [ class "icon"
                            , draggable "false"
                            , src <|
                                "/images/icons/bookmark"
                                    ++ (if isMarked then
                                            "-filled"

                                        else
                                            ""
                                       )
                                    ++ ".svg"
                            ]
                            []
                        , div
                            [ class "hint left swapRight" ]
                            [ text <|
                                (if isMarked then
                                    "Unmark"

                                 else
                                    "Mark"
                                )
                                    ++ " as last reviewed"
                            ]
                        ]
                , if perma then
                    null

                  else
                    a
                        [ class "page", href <| excerptToRoute books excerpt ]
                        [ text <|
                            if excerpt.page == -1 then
                                "¶"

                            else
                                "p. " ++ fromInt excerpt.page
                        , div [ class "hint left swapRight" ] [ text "Permalink" ]
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
                                    excerpt
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

                                    Lenses _ _ ->
                                        "Lenses"

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
                    ((if showLensTab then
                        [ case activeTab of
                            Lenses lensType index ->
                                Lenses lensType index

                            _ ->
                                Lenses Succint 0
                        ]

                      else
                        []
                     )
                        ++ [ Related
                           , Notes
                           , Etc
                           ]
                    )
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
                                            null

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

                        Lenses activeLens lensIndex ->
                            section
                                [ class "lenses" ]
                                [ div
                                    [ class "modeHeading" ]
                                    [ ul
                                        []
                                        (map
                                            (\lens ->
                                                li
                                                    [ classList
                                                        [ ( "active"
                                                          , lens == activeLens
                                                          )
                                                        ]
                                                    ]
                                                    [ button
                                                        [ onClick <|
                                                            SetExcerptTab
                                                                excerpt
                                                                (Lenses lens lensIndex)
                                                                False
                                                        ]
                                                        [ span []
                                                            [ text <|
                                                                case lens of
                                                                    Succint ->
                                                                        "⅀ Succint"

                                                                    Metaphor ->
                                                                        "≈ Metaphor"
                                                            ]
                                                        ]
                                                    ]
                                            )
                                            [ Succint, Metaphor ]
                                        )
                                    ]
                                , div []
                                    (case
                                        excerpt.lenses
                                            |> Dict.fromList
                                            |> get (lensToString activeLens)
                                            |> andThen head
                                     of
                                        Just lensText ->
                                            [ p [] [ text lensText ]
                                            , details
                                                []
                                                [ summary [] [ text "Lenses?" ]
                                                , p
                                                    []
                                                    [ text "Lenses are an upcoming feature of "
                                                    , em [] [ text "Monk-Mode" ]
                                                    , text " which use generative AI to rephrase and summarize ideas."
                                                    ]
                                                ]
                                            ]

                                        _ ->
                                            [ p
                                                [ class "loading" ]
                                                [ text "Loading…" ]
                                            ]
                                    )
                                ]

                        Notes ->
                            section
                                [ class "notes" ]
                                [ textarea
                                    [ onInput <| UpdateNotes excerpt.id
                                    , value excerpt.notes
                                    , placeholder "Add notes here"
                                    ]
                                    [ text excerpt.notes ]
                                ]

                        Etc ->
                            section
                                []
                                [ actionButton
                                    [ onClick <|
                                        ShowConfirmation
                                            "Delete this excerpt?"
                                            (DeleteExcerpt excerpt)
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
