module Views.SearchResults exposing (searchResults)

import Dict exposing (get)
import Html exposing (Html, div, h2, p, text, ul)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy4)
import List exposing (filterMap, isEmpty, length, map)
import Maybe exposing (andThen)
import Model exposing (Book, BookMap, BookSort(..), Entry, EntryMap, Id)
import Msg exposing (Msg)
import Utils exposing (null)
import Views.BookList exposing (bookList)
import Views.Snippet exposing (snippetView)


searchResults :
    BookMap
    -> EntryMap
    -> List Book
    -> List Entry
    -> List ( Id, Float )
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
                    [ h2
                        []
                        [ text <|
                            "Text matches "
                                ++ "("
                                ++ (entries |> length |> String.fromInt)
                                ++ ")"
                        ]
                    , ul
                        []
                        (map
                            (lazy4 snippetView bookMap Nothing (Just query))
                            entries
                        )
                    ]

                  else
                    []
                 )
                    ++ (if not (isEmpty semanticMatches) then
                            [ h2 [] [ text "Semantic matches" ]
                            , ul
                                []
                                (map
                                    (\( entry, score ) ->
                                        snippetView
                                            bookMap
                                            (Just score)
                                            (Just query)
                                            entry
                                    )
                                    (filterMap
                                        (\( id, score ) ->
                                            get id entryMap
                                                |> andThen
                                                    (\entry ->
                                                        Just
                                                            ( entry, score )
                                                    )
                                        )
                                        semanticMatches
                                    )
                                )
                            ]

                        else
                            []
                       )
                )
            ]
