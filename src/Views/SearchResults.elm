module Views.SearchResults exposing (searchResults)

import Dict exposing (get)
import Html exposing (Html, button, div, li, p, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import List exposing (filterMap, isEmpty, length, map, take)
import Msg exposing (Msg(..))
import Types
    exposing
        ( Book
        , BookMap
        , BookSort(..)
        , CountMap
        , Excerpt
        , ExcerptMap
        , ScorePairs
        , SearchMode(..)
        )
import Utils exposing (juxt, null)
import Views.BookList exposing (bookList)
import Views.Snippet exposing (snippetView)


maxResults : Int
maxResults =
    100


searchResults :
    SearchMode
    -> BookMap
    -> ExcerptMap
    -> List Book
    -> List Excerpt
    -> ScorePairs
    -> CountMap
    -> CountMap
    -> String
    -> Html Msg
searchResults mode bookMap excerptMap books matches semanticMatches excerptCounts favCounts query =
    let
        textMatches =
            map (juxt identity (always Nothing)) matches

        semMatches =
            filterMap
                (\( id, score ) ->
                    get id excerptMap
                        |> Maybe.map
                            (\excerpt ->
                                ( excerpt, Just score )
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
            bookList books excerptCounts favCounts TitleSort False
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
                                [ button []
                                    [ span [] [ text title ]
                                    , div [ class "count" ]
                                        [ text <|
                                            if len > maxResults then
                                                ">" ++ String.fromInt maxResults

                                            else
                                                String.fromInt len
                                        ]
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
                let
                    q =
                        if mode == TextMatches then
                            Just query

                        else
                            Nothing
                in
                Keyed.ul
                    []
                    (map
                        (\( excerpt, mScore ) ->
                            ( excerpt.id
                            , snippetView bookMap mScore q excerpt
                            )
                        )
                        (take maxResults list)
                    )
            ]
        ]
