module Views.Base exposing (view)

import Dict exposing (Dict, get, size)
import File
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , footer
        , h1
        , h2
        , h3
        , hr
        , img
        , li
        , main_
        , span
        , sup
        , text
        , ul
        )
import Html.Attributes exposing (class, classList, draggable, href, id, src)
import Html.Events exposing (onClick, preventDefaultOn)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import List exposing (filter, foldl, isEmpty, length, map, reverse, sortBy)
import Maybe exposing (withDefault)
import Model
    exposing
        ( BookMap
        , BookSort(..)
        , EntrySort(..)
        , EntryTab(..)
        , InputFocus(..)
        , Model
        , Page(..)
        , Tag
        , TagSort(..)
        )
import Msg exposing (Msg(..))
import Router exposing (tagToRoute)
import Set
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
import Views.EmbeddingProgress exposing (embeddingProgress)
import Views.Entry exposing (entryView)
import Views.EntryList exposing (entryList)
import Views.Import exposing (importView)
import Views.Landing exposing (landingView)
import Views.SearchInput exposing (searchInput)
import Views.SearchResults exposing (searchResults)
import Views.Settings exposing (settingsView)


view : Model -> Html Msg
view model =
    div
        [ id "root"
        , on "dragenter" (Decode.succeed DragEnter)
        , on "dragover" (Decode.succeed DragEnter)
        , on "dragleave" (Decode.succeed DragLeave)
        , on "drop" dropDecoder
        ]
        ((case model.page of
            LandingPage ->
                [ landingView ]

            _ ->
                [ a
                    [ class "logo", href "/" ]
                    [ img
                        [ src "/images/logo.svg", draggable "false" ]
                        []
                    , case model.page of
                        MainPage _ _ ->
                            null

                        _ ->
                            div
                                [ class "hint" ]
                                [ text "Back to the index" ]
                    ]
                , div
                    [ class "actions" ]
                    [ a
                        [ href "/settings" ]
                        [ img [ src "/images/focus.svg" ] []
                        , div [ class "hint left" ] [ text "Settings" ]
                        ]
                    , button
                        [ class "random"
                        , onClick ShowRandom
                        ]
                        [ img [ src "/images/random.svg" ] []
                        , div
                            [ class "hint left" ]
                            [ text "Discover a random excerpt" ]
                        ]
                    , button
                        [ onClick ScrollToTop ]
                        [ span [] [ text "↟" ]
                        , div
                            [ class "hint left" ]
                            [ text "Scroll to top" ]
                        ]
                    ]
                , if model.demoMode && model.page /= ImportPage then
                    div
                        [ class "demoNotice" ]
                        [ span [] [ text "❧" ]
                        , a
                            [ href "/import" ]
                            [ text "Ready to try Marginalia with your own collection?" ]
                        ]

                  else
                    null
                , main_
                    []
                    [ let
                        completedCount =
                            Set.size model.completedEmbeddings

                        totalCount =
                            size model.entries

                        progressView =
                            if
                                model.embeddingsReady
                                    || completedCount
                                    == 0
                                    || completedCount
                                    >= totalCount
                            then
                                Nothing

                            else
                                Just <|
                                    embeddingProgress
                                        completedCount
                                        totalCount
                      in
                      case model.page of
                        MainPage books mTag ->
                            div
                                [ class "fullWidth" ]
                                [ searchInput model.searchQuery
                                , tagHeader
                                    (mTag /= Nothing || model.showTagHeader)
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

                        SearchPage query semanticReady books entries semanticMatches ->
                            div
                                [ class "searchPage fullWidth" ]
                                [ searchInput model.searchQuery
                                , searchResults
                                    model.books
                                    model.entries
                                    books
                                    entries
                                    semanticMatches
                                    semanticReady
                                    query
                                ]

                        TitlePage book entries ->
                            let
                                entries_ =
                                    if model.entrySort == EntryFavSort then
                                        filter .isFavorite entries

                                    else
                                        entries
                            in
                            div
                                []
                                [ bookInfo
                                    book
                                    model.books
                                    model.tags
                                    model.pendingTag
                                    model.bookNeighborMap
                                    (get book.id model.bookmarks)
                                    model.entrySort
                                    progressView
                                , if isEmpty entries_ then
                                    div
                                        [ class "noFav" ]
                                        [ text "No favorites yet" ]

                                  else
                                    null
                                , entryList
                                    entries_
                                    model.entries
                                    model.books
                                    model.neighborMap
                                    model.idToShowDetails
                                    model.idToActiveTab
                                    (get book.id model.bookmarks
                                        |> withDefault ""
                                    )
                                    progressView
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
                                        (withDefault
                                            []
                                            (get
                                                entry.id
                                                model.neighborMap
                                            )
                                        )
                                        True
                                        (withDefault
                                            Related
                                            (get
                                                entry.id
                                                model.idToActiveTab
                                            )
                                        )
                                        -1
                                        True
                                        False
                                        progressView
                                        entry
                                    ]
                                ]

                        NotFoundPage msg ->
                            div [ class "notFound" ]
                                [ h2 [] [ text "Alas!" ]
                                , h3 [] [ text msg ]
                                , a
                                    [ href "/" ]
                                    [ text "Return to the index." ]
                                ]

                        SettingsPage ->
                            settingsView
                                (size model.entries)
                                (size model.books)
                                (size model.authorRouteMap)
                                (length model.tags)
                                model.semanticThreshold

                        ImportPage ->
                            importView
                                (model.demoMode
                                    || Dict.isEmpty model.entries
                                )

                        _ ->
                            null
                    , footer [] [ text "❦" ]
                    ]
                ]
         )
            ++ [ if model.isDragging then
                    div [ class "dragNotice" ] [ text "Drop your file here." ]

                 else
                    text ""
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
                [ RecencySort, TitleSort, RatingSort, NumSort, FavSort ]
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
            [ class "tabs" ]
            [ button
                [ onClick ToggleTagHeader, class "active" ]
                [ text "Tags" ]
            ]
        , if show then
            div
                []
                [ ul
                    [ class "modeHeading" ]
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
                , Keyed.ul
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
                                                get tag tagCounts
                                                    |> withDefault 0
                                            )
                                        |> reverse

                                else
                                    tags
                               )
                        )
                    )
                ]

          else
            null
        , hr [] []
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

        RatingSort ->
            "Rating"

        NumSort ->
            "№ excerpts"

        FavSort ->
            "№ favorites"


sortToBounds : BookSort -> List String
sortToBounds sort =
    case sort of
        RecencySort ->
            [ "older", "newer" ]

        TitleSort ->
            [ "A", "Z" ]

        RatingSort ->
            [ "worse", "better" ]

        _ ->
            [ "less", "more" ]


dropDecoder : Decoder Msg
dropDecoder =
    Decode.at
        [ "dataTransfer", "files" ]
        (Decode.oneOrMore (GotFiles FileLoad) File.decoder)


on : String -> Decoder msg -> Attribute msg
on event decoder =
    preventDefaultOn event (Decode.map (\m -> ( m, True )) decoder)
