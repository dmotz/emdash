module Views.Base exposing (view)

import Dict exposing (get, size)
import File
import Html
    exposing
        ( Html
        , a
        , button
        , details
        , div
        , footer
        , h2
        , h3
        , li
        , main_
        , p
        , progress
        , span
        , summary
        , sup
        , text
        , ul
        )
import Html.Attributes exposing (attribute, class, classList, href, id, value)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3, lazy4)
import Json.Decode as Decode exposing (Decoder)
import List exposing (isEmpty, map, reverse, sortBy)
import Maybe exposing (andThen, withDefault)
import Model
    exposing
        ( BookSort(..)
        , EntryTab(..)
        , Filter(..)
        , InputFocus(..)
        , Model
        , TagSort(..)
        )
import Msg exposing (Msg(..))
import Router exposing (tagToRoute)
import Set
import String exposing (fromFloat, join)
import Utils exposing (formatNumber, pluckIds, untaggedKey)
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
        [ lazy3 headerView model.filter model.searchQuery model.hideHeader
        , main_ []
            [ case model.notFoundMsg of
                Just msg ->
                    div [ class "notFound" ]
                        [ h2 [] [ text "Alas!" ]
                        , h3 [] [ text msg ]
                        , a [ href "/" ] [ text "Return to the index." ]
                        ]

                _ ->
                    case
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
                                                pluckIds
                                                    model.books
                                                    model.booksShown
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
                                                    (pluckIds
                                                        model.entries
                                                        entryIds
                                                    )
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
                                                |> andThen
                                                    (\id ->
                                                        get
                                                            id
                                                            model.books
                                                    )
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
                                [ let
                                    done =
                                        Set.size model.completedEmbeddings

                                    needed =
                                        size model.entries
                                  in
                                  if
                                    not model.embeddingsReady
                                        && (done /= 0)
                                        && (done /= needed)
                                  then
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
                                , case model.filter of
                                    Just (TagFilter _) ->
                                        tagHeader model

                                    Nothing ->
                                        tagHeader model

                                    _ ->
                                        text ""
                                , div
                                    [ class "bookSort" ]
                                    [ div [] [ text "sort by:" ]
                                    , ul []
                                        (map
                                            (\sort ->
                                                li
                                                    [ classList
                                                        [ ( "active"
                                                          , sort == model.bookSort
                                                          )
                                                        ]
                                                    ]
                                                    [ button
                                                        [ onClick <|
                                                            SortBooks sort
                                                        ]
                                                        [ text <|
                                                            sortToString sort
                                                        ]
                                                    ]
                                            )
                                            [ RecencySort, TitleSort, NumSort ]
                                        )
                                    , div [] [ text "|" ]
                                    , button
                                        [ onClick Sort ]
                                        (let
                                            ( arrow, f ) =
                                                if model.reverseSort then
                                                    ( "▲", identity )

                                                else
                                                    ( "▼", reverse )
                                         in
                                         [ span [] [ text arrow ]
                                         , model.bookSort
                                            |> sortToBounds
                                            |> f
                                            |> join "–"
                                            |> text
                                         ]
                                        )
                                    ]
                                , bookList <|
                                    pluckIds model.books model.booksShown
                                ]
            , button [ class "scrollToTop", onClick ScrollToTop ] [ text "⇞" ]
            , footer [] [ text "❦" ]
            ]
        ]


tagHeader : Model -> Html Msg
tagHeader model =
    details
        [ class "tagHeader", attribute "open" "true" ]
        [ summary [] [ text "Tags" ]
        , ul
            [ class "tagSort" ]
            (map
                (\sort ->
                    li
                        []
                        [ button
                            [ onClick <| SetTagSort sort
                            , classList
                                [ ( "active"
                                  , sort == model.tagSort
                                  )
                                ]
                            ]
                            (case sort of
                                TagAlphaSort ->
                                    [ text "▲"
                                    , span [] [ text "A–Z" ]
                                    ]

                                _ ->
                                    [ text "▼"
                                    , span [] [ text "№" ]
                                    ]
                            )
                        ]
                )
                [ TagAlphaSort, TagNumSort ]
            )
        , Keyed.ul
            [ class "tags" ]
            (map
                (\tag ->
                    ( tag
                    , li
                        [ class "tag"
                        , classList
                            [ ( "active"
                              , case model.filter of
                                    Just (TagFilter t) ->
                                        tag == t

                                    _ ->
                                        tag == allBooksKey
                              )
                            , ( "special"
                              , tag
                                    == allBooksKey
                                    || tag
                                    == untaggedKey
                              )
                            ]
                        ]
                        [ a
                            [ href <|
                                if tag == allBooksKey then
                                    "/"

                                else
                                    tagToRoute tag
                            ]
                            [ text tag ]
                        , sup
                            [ class "count" ]
                            [ text <|
                                if model.tagSort == TagNumSort then
                                    if tag == allBooksKey then
                                        model.books
                                            |> size
                                            |> formatNumber

                                    else
                                        get tag model.tagCounts
                                            |> withDefault 0
                                            |> formatNumber

                                else
                                    ""
                            ]
                        ]
                    )
                )
                ([ allBooksKey, untaggedKey ]
                    ++ (if model.tagSort == TagNumSort then
                            model.tags
                                |> sortBy
                                    (\tag ->
                                        get tag
                                            model.tagCounts
                                            |> withDefault 0
                                    )
                                |> reverse

                        else
                            model.tags
                       )
                )
            )
        ]


allBooksKey : String
allBooksKey =
    "all books"


sortToString : BookSort -> String
sortToString sort =
    case sort of
        RecencySort ->
            "recent"

        TitleSort ->
            "title"

        _ ->
            "№ excerpts"


sortToBounds sort =
    case sort of
        RecencySort ->
            [ "older", "newer" ]

        TitleSort ->
            [ "A", "Z" ]

        _ ->
            [ "less", "more" ]


dropDecoder : Decoder Msg
dropDecoder =
    Decode.at
        [ "dataTransfer", "files" ]
        (Decode.oneOrMore (GotFiles FileLoad) File.decoder)
