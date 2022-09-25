module Views.SearchResults exposing (searchResults)

import Dict exposing (get)
import Html exposing (Html, div, p, text, ul)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy4)
import List exposing (filterMap, isEmpty, map)
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
    -> String
    -> Html Msg
searchResults bookMap entryMap books entries semanticMatches query =
    div
        [ class "searchResults" ]
        [ if isEmpty books && isEmpty entries then
            p
                [ class "noResults" ]
                [ text "No results found." ]

          else if isEmpty books then
            null

          else
            bookList books TitleSort False
        , if isEmpty entries then
            null

          else
            ul
                [ class "snippets" ]
                (map
                    (lazy4 snippetView bookMap Nothing (Just query))
                    entries
                    ++ map
                        (\( entry, score ) ->
                            snippetView bookMap (Just score) (Just query) entry
                        )
                        (filterMap
                            (\( id, score ) ->
                                get id entryMap
                                    |> andThen (\entry -> Just ( entry, score ))
                            )
                            semanticMatches
                        )
                )
        ]
