module Views.SearchResults exposing (searchResults)

import Dict exposing (get)
import Html exposing (Html, div, h2, p, sup, text, ul)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import List exposing (filterMap, isEmpty, length, map)
import Model exposing (Book, BookMap, BookSort(..), Entry, EntryMap, ScorePairs)
import Msg exposing (Msg)
import Utils exposing (juxt, null)
import Views.BookList exposing (bookList)
import Views.Snippet exposing (snippetView)


searchResults :
    BookMap
    -> EntryMap
    -> List Book
    -> List Entry
    -> ScorePairs
    -> Bool
    -> String
    -> Html Msg
searchResults bookMap entryMap books entries semanticMatches semanticReady query =
    if isEmpty books && isEmpty entries && isEmpty semanticMatches && semanticReady then
        p
            [ class "noResults" ]
            [ text "No results found." ]

    else
        div
            [ class "searchResults" ]
            [ if isEmpty books then
                null

              else
                bookList books TitleSort False
            , div
                [ class "snippets" ]
                ((if not (isEmpty entries) then
                    [ resultsSection
                        "Text matches"
                        query
                        bookMap
                        (map (juxt identity (always Nothing)) entries)
                    ]

                  else
                    []
                 )
                    ++ (if not (isEmpty semanticMatches) then
                            [ resultsSection
                                "Semantic matches"
                                query
                                bookMap
                                (filterMap
                                    (\( id, score ) ->
                                        get id entryMap
                                            |> Maybe.map
                                                (\entry ->
                                                    ( entry, Just score )
                                                )
                                    )
                                    semanticMatches
                                )
                            ]

                        else
                            []
                       )
                )
            ]


resultsSection : String -> String -> BookMap -> List ( Entry, Maybe Float ) -> Html Msg
resultsSection title query bookMap items =
    div
        []
        [ h2
            []
            [ text title
            , sup [] [ items |> length |> String.fromInt |> text ]
            ]
        , Keyed.ul
            []
            (map
                (\( entry, mScore ) ->
                    ( entry.id, snippetView bookMap mScore (Just query) entry )
                )
                items
            )
        ]
