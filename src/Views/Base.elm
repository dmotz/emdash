module Views.Base exposing (view)

import Dict exposing (Dict, get, size)
import File
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h1
        , h2
        , h3
        , img
        , li
        , main_
        , span
        , sup
        , text
        , ul
        )
import Html.Attributes exposing (class, classList, draggable, href, id, src)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import List exposing (foldl, length, map, reverse, sortBy)
import Maybe exposing (withDefault)
import Model
    exposing
        ( BookMap
        , BookSort(..)
        , EntryTab(..)
        , Filter(..)
        , InputFocus(..)
        , Model
        , Page(..)
        , Tag
        , TagSort(..)
        )
import Msg exposing (Msg(..))
import Router exposing (tagToRoute)
import String exposing (fromFloat, join)
import Utils
    exposing
        ( excerptCountLabel
        , formatNumber
        , titleCountLabel
        , untaggedKey
        )
import Views.BookInfo exposing (bookInfo)
import Views.BookList exposing (bookList)
import Views.Common exposing (on)
import Views.Entry exposing (entryView)
import Views.EntryList exposing (entryList)
import Views.Landing exposing (landingView)
import Views.SearchInput exposing (searchInput)
import Views.SearchResults exposing (searchResults)


view : Model -> Html Msg
view model =
    div
        [ id "root"
        , on "dragenter" (Decode.succeed DragEnter)
        , on "dragover" (Decode.succeed DragEnter)
        , on "dragleave" (Decode.succeed DragLeave)
        , on "drop" dropDecoder
        ]
        -- ((if model.isDragging then
        --     div [] [ text "dragging" ]
        --   else
        --     text ""
        --  )
        --     ::
        (if Dict.isEmpty model.books then
            [ landingView ]

         else
            [ a
                [ class "logo", href "/" ]
                [ img
                    [ src "/logo.svg"
                    , draggable "false"
                    , onClick ToggleAboutMode
                    ]
                    []
                ]
            , div
                [ class "actions" ]
                [ a
                    [ href "/settings" ]
                    [ img [ src "/focus.svg" ] [] ]
                , button
                    [ onClick ShowRandom ]
                    [ img [ src "/random.svg" ] [] ]
                ]
            , main_ []
                [ searchInput model.searchQuery
                , case model.notFoundMsg of
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
                                        searchResults
                                            model.books
                                            model.booksShown
                                            model.entries
                                            entryIds
                                            model.semanticMatches
                                            query

                                    _ ->
                                        div []
                                            (case
                                                model.currentBook
                                                    |> andThen
                                                        (\id ->
                                                            get
                                                                id
                                                                model.books
                                                        )
                                             of
                                                Just book ->
                                                    [ bookInfo
                                                        book
                                                        model.books
                                                        model.tags
                                                        model.pendingTag
                                                        model.bookNeighborMap
                                                        (length entryIds)
                                                    , entryList
                                                        entryIds
                                                        model.entries
                                                        model.books
                                                        model.neighborMap
                                                        model.idToShowDetails
                                                        model.idToActiveTab
                                                    ]

                                                _ ->
                                                    [ case
                                                        head entryIds
                                                            |> andThen
                                                                (\id ->
                                                                    get
                                                                        id
                                                                        model.entries
                                                                )
                                                      of
                                                        Just entry ->
                                                            entryView
                                                                model.entries
                                                                model.books
                                                                model.neighborMap
                                                                True
                                                                Related
                                                                1
                                                                entry

                                                        Nothing ->
                                                            text ""
                                                    ]
                                            )

                            _ ->
                                let
                                    books =
                                        pluckIds model.books model.booksShown
                                in
                                div
                                    [ class "books" ]
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
                                        Just (AuthorFilter author) ->
                                            div
                                                [ class "authorInfo" ]
                                                [ h1 [] [ text author ]
                                                , h2
                                                    []
                                                    [ titleCountLabel
                                                        (length books)
                                                        ++ ", "
                                                        ++ (books
                                                                |> foldl
                                                                    (\{ count } acc ->
                                                                        acc + count
                                                                    )
                                                                    0
                                                                |> excerptCountLabel
                                                           )
                                                        |> text
                                                    ]
                                                ]

                                        Just (TagFilter _) ->
                                            tagHeader model

                                        Nothing ->
                                            tagHeader model

                                        _ ->
                                            text ""
                                    , hr [] []
                                    , bookSorter
                                        model.bookSort
                                        model.reverseSort
                                    , bookList books
                                    ]
                , footer [] [ text "❦" ]
                ]
            ]
        )


bookSorter : BookSort -> Bool -> Html Msg
bookSorter activeSort reverseSort =
    div
        [ class "modeHeading" ]
        [ --div [] [ text "Sorting:" ]
          ul
            []
            (map
                (\sort ->
                    li
                        [ classList [ ( "active", sort == activeSort ) ] ]
                        [ button
                            [ onClick <| SortBooks sort ]
                            [ text <| sortToString sort ]
                        ]
                )
                [ RecencySort, TitleSort, NumSort ]
            )
        , div [ class "divider" ] [ text "|" ]
        , div []
            [ button
                [ onClick Sort ]
                (let
                    ( arrow, f ) =
                        if reverseSort then
                            ( "▼", reverse )

                        else
                            ( "▲", identity )
                 in
                 [ span [] [ text arrow ]
                 , activeSort |> sortToBounds |> f |> join "–" |> text
                 ]
                )
            ]
        ]


tagHeader :
    Bool
    -> BookMap
    -> TagSort
    -> List Tag
    -> Dict Tag Int
    -> Maybe Tag
    -> Html Msg
tagHeader show allBooks tagSort tags tagCounts mActiveTag =
    div
        [ class "tagHeader" ]
        [ div
            [ class "modeHeading" ]
            [ -- div [] [ text "Tags:" ]
              ul
                []
                [ li
                    [ classList [ ( "active", show ) ] ]
                    [ button [ onClick ToggleTagHeader ] [ text "Show tags" ] ]
                , li
                    [ classList [ ( "active", not show ) ] ]
                    [ button [ onClick ToggleTagHeader ] [ text "Hide tags" ] ]
                ]

            -- , div [ class "divider" ] [ text "|" ]
            , if show then
                ul
                    [ class "last" ]
                    (map
                        (\sort ->
                            li
                                [ classList [ ( "active", sort == tagSort ) ] ]
                                [ button
                                    [ onClick <| SetTagSort sort ]
                                    [ text <|
                                        case sort of
                                            TagAlphaSort ->
                                                "A–Z"

                                            TagNumSort ->
                                                "№ excerpts"
                                    ]
                                ]
                        )
                        [ TagAlphaSort, TagNumSort ]
                    )

              else
                text ""
            ]
        , if show then
            Keyed.ul
                [ class "tags" ]
                (map
                    (\tag ->
                        ( tag
                        , li
                            [ class "tag"
                            , classList
                                [ ( "active"
                                  , case mActiveTag of
                                        Just t ->
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
                            , if tagSort == TagNumSort then
                                sup
                                    [ class "count" ]
                                    [ text <|
                                        if tag == allBooksKey then
                                            allBooks |> size |> formatNumber

                                        else
                                            get tag tagCounts
                                                |> withDefault 0
                                                |> formatNumber
                                    ]

                              else
                                text ""
                            ]
                        )
                    )
                    ([ allBooksKey, untaggedKey ]
                        ++ (if tagSort == TagNumSort then
                                tags
                                    |> sortBy
                                        (\tag ->
                                            get tag tagCounts |> withDefault 0
                                        )
                                    |> reverse

                            else
                                tags
                           )
                    )
                )

          else
            text ""
        ]


allBooksKey : String
allBooksKey =
    "all books"


sortToString : BookSort -> String
sortToString sort =
    case sort of
        RecencySort ->
            "Recent"

        TitleSort ->
            "Title"

        _ ->
            "№ excerpts"


sortToBounds : BookSort -> List String
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
