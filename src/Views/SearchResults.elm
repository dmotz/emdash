module Views.SearchResults exposing (searchResults)

import Dict exposing (get)
import Html exposing (Html, div, p, text, ul)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy4)
import List exposing (filterMap, isEmpty, map)
import Maybe exposing (andThen)
import Model exposing (BookMap, EntryMap, Id)
import Msg exposing (Msg)
import Utils exposing (pluckIds)
import Views.BookList exposing (bookList)
import Views.Snippet exposing (snippetView)


searchResults :
    BookMap
    -> List Id
    -> EntryMap
    -> List Id
    -> List ( Id, Float )
    -> String
    -> Html Msg
searchResults books bookIds entries entryIds semanticMatches query =
    div
        [ class "searchResults" ]
        [ if isEmpty bookIds then
            text ""

          else
            bookList <| pluckIds books bookIds
        , if isEmpty entryIds then
            text ""

          else
            ul
                [ class "snippets" ]
                (map
                    (lazy4 snippetView books Nothing (Just query))
                    (pluckIds entries entryIds)
                    ++ map
                        (\( entry, score ) ->
                            snippetView books (Just score) (Just query) entry
                        )
                        (filterMap
                            (\( id, score ) ->
                                get id entries
                                    |> andThen (\entry -> Just ( entry, score ))
                            )
                            semanticMatches
                        )
                )
        , if isEmpty bookIds && isEmpty entryIds then
            p
                [ class "noResults" ]
                [ text "No results found." ]

          else
            text ""
        ]
