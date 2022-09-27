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
        , InputFocus(..)
        , Model
        , Page(..)
        , Tag
        , TagSort(..)
        )
import Msg exposing (Msg(..))
import Router exposing (tagToRoute)
import String exposing (join)
import Utils
    exposing
        ( excerptCountLabel
        , formatNumber
        , null
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
        (case model.page of
            LandingPage ->
                [ landingView ]

            _ ->
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
                , main_
                    []
                    [ case model.page of
                        MainPage books mTag ->
                            div
                                [ class "fullWidth" ]
                                [ searchInput model.searchQuery
                                , tagHeader
                                    model.showTagHeader
                                    model.books
                                    model.tagSort
                                    model.tags
                                    model.tagCounts
                                    mTag
                                , bookSorter
                                    model.bookSort
                                    model.reverseSort
                                , bookList
                                    books
                                    model.bookSort
                                    model.reverseSort
                                ]

                        SearchPage query books entries ->
                            div
                                [ class "searchPage" ]
                                [ searchInput model.searchQuery
                                , searchResults
                                    model.books
                                    model.entries
                                    books
                                    entries
                                    model.semanticMatches
                                    query
                                ]

                        TitlePage book entries ->
                            div
                                []
                                [ bookInfo
                                    book
                                    model.books
                                    model.tags
                                    model.pendingTag
                                    model.bookNeighborMap
                                    (length entries)
                                , entryList
                                    entries
                                    model.entries
                                    model.books
                                    model.neighborMap
                                    model.idToShowDetails
                                    model.idToActiveTab
                                ]

                        AuthorPage author books ->
                            div []
                                [ div
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
                                , bookSorter
                                    model.bookSort
                                    model.reverseSort
                                , bookList
                                    books
                                    model.bookSort
                                    model.reverseSort
                                ]

                        EntryPage entry _ ->
                            div
                                []
                                [ ul
                                    [ class "entries" ]
                                    [ entryView
                                        model.entries
                                        model.books
                                        model.neighborMap
                                        True
                                        (withDefault
                                            Related
                                            (get entry.id model.idToActiveTab)
                                        )
                                        -1
                                        True
                                        entry
                                    ]
                                ]

                        NotFoundPage msg ->
                            div [ class "notFound" ]
                                [ h2 [] [ text "Alas!" ]
                                , h3 [] [ text msg ]
                                , a [ href "/" ] [ text "Return to the index." ]
                                ]

                        _ ->
                            null
                    ]
                ]
        )


bookSorter : BookSort -> Bool -> Html Msg
bookSorter activeSort reverseSort =
    div
        [ class "modeHeading" ]
        [ ul
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
                null
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
                                null
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
            null
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
