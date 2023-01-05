module Views.SearchResults exposing (searchResults)

import Dict exposing (get)
import Html exposing (Html, button, div, li, p, sup, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import List exposing (filterMap, isEmpty, length, map, take)
import Model
    exposing
        ( Book
        , BookMap
        , BookSort(..)
        , Entry
        , EntryMap
        , ScorePairs
        , SearchMode(..)
        )
import Msg exposing (Msg(..))
import Utils exposing (juxt, null)
import Views.BookList exposing (bookList)
import Views.Snippet exposing (snippetView)


maxResults : Int
maxResults =
    100


searchResults :
    SearchMode
    -> BookMap
    -> EntryMap
    -> List Book
    -> List Entry
    -> ScorePairs
    -> String
    -> Html Msg
searchResults mode bookMap entryMap books matches semanticMatches query =
    let
        textMatches =
            map (juxt identity (always Nothing)) matches

        semMatches =
            filterMap
                (\( id, score ) ->
                    get id entryMap
                        |> Maybe.map
                            (\entry ->
                                ( entry, Just score )
                            )
                )
                semanticMatches

        ( list, label ) =
            if mode == TextMatches then
                ( textMatches, "text matches" )

            else
                ( semMatches, "semantic matches" )
    in
    div
        [ class "searchResults" ]
        [ if isEmpty books then
            null

          else
            bookList books TitleSort False
        , div
            [ class "snippets" ]
            [ div
                [ class "modeHeading" ]
                [ ul
                    []
                    (map
                        (\( m, title, len ) ->
                            li
                                [ classList [ ( "active", m == mode ) ]
                                , onClick (SetSearchTab m)
                                ]
                                [ button [] [ text title ]
                                , sup []
                                    [ text <|
                                        if len > maxResults then
                                            ">" ++ String.fromInt maxResults

                                        else
                                            String.fromInt len
                                    ]
                                ]
                        )
                        [ ( TextMatches, "Text matches", length textMatches )
                        , ( SemanticMatches, "Semantic matches", length semMatches )
                        ]
                    )
                ]
            , if isEmpty list then
                p
                    [ class "noResults" ]
                    [ text <| "No " ++ label ++ " found." ]

              else
                Keyed.ul
                    []
                    (map
                        (\( entry, mScore ) ->
                            ( entry.id
                            , snippetView bookMap mScore (Just query) entry
                            )
                        )
                        (take maxResults list)
                    )
            ]
        ]
