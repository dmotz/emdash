module Views.Base exposing (view)

import Dict exposing (get)
import File
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , footer
        , li
        , main_
        , p
        , progress
        , text
        , ul
        )
import Html.Attributes exposing (class, href, id, value)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy4)
import Json.Decode as Decode exposing (Decoder)
import List exposing (isEmpty, map)
import Maybe exposing (andThen)
import Model
    exposing
        ( BookSort(..)
        , EntryTab(..)
        , Filter(..)
        , InputFocus(..)
        , Model
        )
import Msg exposing (Msg(..))
import Router exposing (tagToRoute)
import Set exposing (size)
import String exposing (fromFloat)
import Utils exposing (formatNumber, pluckIds)
import Views.BookInfo exposing (bookInfo)
import Views.BookList exposing (bookList)
import Views.Common exposing (on)
import Views.EntryList exposing (entryList)
import Views.Header exposing (headerView)
import Views.Snippet exposing (snippetView)


view : Model -> Html Msg
view model =
    div
        [ id "root"
        , on "dragenter" (Decode.succeed DragEnter)
        , on "dragover" (Decode.succeed DragEnter)
        , on "dragleave" (Decode.succeed DragLeave)
        , on "drop" dropDecoder
        ]
        [ lazy4
            headerView
            model.filter
            model.searchQuery
            model.reverseSort
            model.hideHeader
        , main_ []
            [ case
                model.entriesShown
              of
                Just entryIds ->
                    case model.filter of
                        Just (TextFilter query) ->
                            div
                                [ class "searchResults" ]
                                [ if isEmpty model.booksShown then
                                    text ""

                                  else
                                    bookList <|
                                        pluckIds model.books model.booksShown
                                , if isEmpty entryIds then
                                    text ""

                                  else
                                    ul
                                        [ class "snippets" ]
                                        (map
                                            (lazy4
                                                snippetView
                                                model.books
                                                Nothing
                                                (Just query)
                                            )
                                            (pluckIds model.entries entryIds)
                                        )
                                , if
                                    isEmpty model.booksShown
                                        && isEmpty entryIds
                                  then
                                    p [ class "noResults" ]
                                        [ text "No results found." ]

                                  else
                                    text ""
                                ]

                        _ ->
                            div []
                                [ case
                                    model.currentBook
                                        |> andThen (\id -> get id model.books)
                                  of
                                    Just book ->
                                        bookInfo
                                            book
                                            model.books
                                            model.tags
                                            model.pendingTag
                                            model.bookNeighborMap

                                    _ ->
                                        text ""
                                , entryList
                                    entryIds
                                    model.entries
                                    model.books
                                    model.neighborMap
                                    model.idToShowDetails
                                    model.idToActiveTab
                                ]

                _ ->
                    div
                        []
                        [ if
                            not model.embeddingsReady
                                && Dict.size model.entries
                                /= 0
                          then
                            let
                                done =
                                    model.completedEmbeddings |> size

                                needed =
                                    Dict.size model.entries
                            in
                            div
                                []
                                [ progress
                                    [ toFloat done
                                        / toFloat needed
                                        |> fromFloat
                                        |> value
                                    ]
                                    []
                                , text <|
                                    formatNumber done
                                        ++ " / "
                                        ++ formatNumber needed
                                ]

                          else
                            text ""
                        , ul []
                            (map
                                (\tag ->
                                    li
                                        [ class "tag" ]
                                        [ a
                                            [ href <| tagToRoute tag ]
                                            [ text tag ]
                                        ]
                                )
                                model.tags
                            )
                        , bookList <| pluckIds model.books model.booksShown
                        ]
            , button [ class "scrollToTop", onClick ScrollToTop ] [ text "⇞" ]
            , footer [] [ text "❦" ]
            ]
        ]


dropDecoder : Decoder Msg
dropDecoder =
    Decode.at
        [ "dataTransfer", "files" ]
        (Decode.oneOrMore (GotFiles FileLoad) File.decoder)
