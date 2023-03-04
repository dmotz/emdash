module Views.Base exposing (view)

import Dict exposing (Dict, get, size)
import Html
    exposing
        ( Html
        , a
        , aside
        , button
        , code
        , div
        , footer
        , h2
        , h3
        , hr
        , img
        , li
        , main_
        , p
        , span
        , sup
        , text
        , ul
        )
import Html.Attributes exposing (alt, class, classList, draggable, href, id, src, target)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import List exposing (filter, isEmpty, length, map, reverse, sortBy)
import Maybe exposing (withDefault)
import Model
    exposing
        ( BookMap
        , BookSort(..)
        , ExcerptSort(..)
        , ExcerptTab(..)
        , Model
        , Page(..)
        , Tag
        , TagSort(..)
        )
import Msg exposing (Msg(..))
import Router exposing (tagToRoute)
import Set
import String exposing (join)
import Utils exposing (appName, formatNumber, null, untaggedKey)
import Views.AuthorInfo exposing (authorInfo)
import Views.BookInfo exposing (bookInfo)
import Views.BookList exposing (bookList)
import Views.Create exposing (createView)
import Views.EmbeddingProgress exposing (embeddingProgress)
import Views.Excerpt exposing (excerptView)
import Views.ExcerptList exposing (excerptList)
import Views.Import exposing (importView)
import Views.Landing exposing (landingView)
import Views.SearchInput exposing (searchInput)
import Views.SearchResults exposing (searchResults)
import Views.Settings exposing (settingsView)
import Views.Toolbar exposing (toolbar)


view : Model -> Html Msg
view model =
    div
        [ id "root" ]
        ((case model.page of
            LandingPage bookList didSubmitEmail ->
                [ landingView bookList didSubmitEmail ]

            _ ->
                [ a
                    [ class "logo", href "/" ]
                    [ img
                        [ src "/images/logo.svg", draggable "false", alt appName ]
                        []
                    , case model.page of
                        MainPage _ _ ->
                            null

                        _ ->
                            div
                                [ class "hint" ]
                                [ text "Back to the index" ]
                    ]
                , toolbar
                , if model.demoMode && model.page /= ImportPage then
                    div
                        [ class "demoNotice" ]
                        [ aside
                            []
                            [ text "Feel free to peruse this sample library. Make yourself at home." ]
                        , div
                            []
                            [ span [] [ text "❧" ]
                            , a
                                [ href "/import" ]
                                [ text "Ready to try Marginalia with your own collection?" ]
                            ]
                        ]

                  else
                    null
                , main_
                    []
                    [ let
                        completedCount =
                            Set.size model.completedEmbeddings

                        totalCount =
                            size model.excerpts

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

                        SearchPage query mode books excerpts semanticMatches ->
                            div
                                [ class "searchPage fullWidth" ]
                                [ searchInput model.searchQuery
                                , searchResults
                                    mode
                                    model.books
                                    model.excerpts
                                    books
                                    excerpts
                                    semanticMatches
                                    query
                                ]

                        TitlePage book excerpts ->
                            let
                                excerpts_ =
                                    if model.excerptSort == ExcerptFavSort then
                                        filter .isFavorite excerpts

                                    else
                                        excerpts
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
                                    model.excerptSort
                                    progressView
                                , if isEmpty excerpts_ then
                                    div
                                        [ class "noFav" ]
                                        [ text <|
                                            "No "
                                                ++ (if model.excerptSort == ExcerptFavSort then
                                                        "favorites"

                                                    else
                                                        "excerpts"
                                                   )
                                                ++ " yet"
                                        ]

                                  else
                                    null
                                , excerptList
                                    excerpts_
                                    model.excerpts
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
                                [ authorInfo author books
                                , bookSorter
                                    model.bookSort
                                    model.reverseSort
                                , bookList
                                    books
                                    model.bookSort
                                    model.reverseSort
                                ]

                        ExcerptPage excerpt _ ->
                            div
                                []
                                [ ul
                                    [ class "excerpts" ]
                                    [ excerptView
                                        model.excerpts
                                        model.books
                                        (withDefault
                                            []
                                            (get
                                                excerpt.id
                                                model.neighborMap
                                            )
                                        )
                                        True
                                        (withDefault
                                            Related
                                            (get
                                                excerpt.id
                                                model.idToActiveTab
                                            )
                                        )
                                        -1
                                        True
                                        False
                                        progressView
                                        excerpt
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
                                model.version
                                (size model.excerpts)
                                (size model.books)
                                (size model.authorRouteMap)
                                (length model.tags)
                                model.semanticThreshold

                        ImportPage ->
                            importView
                                (model.demoMode
                                    || Dict.isEmpty model.excerpts
                                )
                                model.isDragging

                        CreatePage pExcerpt books authors ->
                            createView pExcerpt books authors

                        _ ->
                            null
                    ]
                ]
         )
            ++ footer
                []
                [ div
                    [ class "links" ]
                    [ a [ href "/import" ] [ text "Import excerpts" ]
                    , if Dict.isEmpty model.excerpts then
                        null

                      else
                        a [ href "/settings" ] [ text "Settings" ]
                    , a
                        [ href "https://github.com/dmotz/marginalia/issues/new"
                        , target "_blank"
                        ]
                        [ text "Report a bug" ]
                    , a
                        [ href "https://github.com/dmotz/marginalia"
                        , target "_blank"
                        ]
                        [ text "Source code" ]
                    ]
                , div [ class "fleuron" ] [ text "❦" ]
                ]
            :: (case model.parsingError of
                    Just msg ->
                        [ div
                            [ class "modal" ]
                            [ div
                                [ class "errorBox" ]
                                [ p [] [ text "An error occurred parsing the file:" ]
                                , div [] [ code [] [ text msg ] ]
                                , button
                                    [ class "button", onClick ResetError ]
                                    [ text "Dismiss" ]
                                ]
                            ]
                        ]

                    _ ->
                        []
               )
        )


bookSorter : BookSort -> Bool -> Html Msg
bookSorter activeSort reverseSort =
    div
        [ class "modeHeading center" ]
        [ ul
            []
            (map
                (\sort ->
                    li
                        [ classList [ ( "active", sort == activeSort ) ] ]
                        [ button
                            [ onClick <| SortBooks sort ]
                            [ span [] [ text <| sortToString sort ] ]
                        , if sort == activeSort then
                            button
                                [ onClick Sort, class "sorter" ]
                                [ span
                                    []
                                    [ span
                                        [ classList
                                            [ ( "arrow", True )
                                            , ( "reverse", reverseSort )
                                            ]
                                        ]
                                        [ text "▲" ]
                                    , activeSort
                                        |> sortToBounds
                                        |> (if reverseSort then
                                                reverse

                                            else
                                                identity
                                           )
                                        |> join "–"
                                        |> text
                                    ]
                                ]

                          else
                            null
                        ]
                )
                [ RecencySort, TitleSort, RatingSort, NumSort, FavSort ]
            )
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
                                    [ span
                                        []
                                        [ text <|
                                            case sort of
                                                TagAlphaSort ->
                                                    "A–Z"

                                                TagNumSort ->
                                                    "№ titles"
                                        ]
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
    "all"


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
