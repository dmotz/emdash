module Views.Base exposing (view)

-- import Regex

import Dict exposing (Dict, get, size)
import Html
    exposing
        ( Html
        , a
        , aside
        , br
        , button
        , code
        , div
        , footer
        , h2
        , h3
        , h4
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
import Model exposing (ModalMsg(..), Model)
import Msg exposing (Msg(..))
import Router exposing (tagToRoute)
import Set
import String exposing (join)
import Types
    exposing
        ( BookMap
        , BookSort(..)
        , ExcerptSort(..)
        , ExcerptTab(..)
        , Lens(..)
        , Page(..)
        , Tag
        , TagSort(..)
        )
import Utils exposing (appName, formatNumber, getCount, null, repoUrl, untaggedKey)
import Views.AuthorInfo exposing (authorInfo)
import Views.BookInfo exposing (bookInfo)
import Views.BookList exposing (bookList)
import Views.Button exposing (actionButton)
import Views.Create exposing (createView)
import Views.EmbeddingProgress exposing (embeddingProgress)
import Views.Excerpt exposing (excerptView)
import Views.ExcerptList exposing (excerptList)
import Views.Import exposing (importView)
import Views.Landing exposing (landingView)
import Views.MonkSignup exposing (monkSignup)
import Views.SearchInput exposing (searchInput)
import Views.SearchResults exposing (searchResults)
import Views.Settings exposing (settingsView)
import Views.Toolbar exposing (toolbar)


view : Model -> Html Msg
view model =
    div
        [ id "root" ]
        ((case model.page of
            LandingPage books countMap ->
                [ landingView books countMap model.didJoinMailingList ]

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
                            [ text "Feel free to peruse this sample library. Make yourself at home."
                            , br [] []
                            , button
                                [ onClick ShowRandom ]
                                [ text "Try viewing a random excerpt." ]
                            ]
                        , div
                            []
                            [ span [] [ text "❧" ]
                            , a
                                [ href "/import" ]
                                [ text <|
                                    "Ready to use "
                                        ++ appName
                                        ++ " with your own collection?"
                                ]
                            ]
                        ]

                  else
                    null
                , main_
                    []
                    [ searchInput model.searchQuery
                    , let
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
                                [ tagHeader
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
                                    model.excerptCountMap
                                    model.favCountMap
                                    model.bookSort
                                    model.reverseSort
                                ]

                        SearchPage query mode books excerpts semanticMatches ->
                            div
                                [ class "searchPage fullWidth" ]
                                [ searchResults
                                    mode
                                    model.books
                                    model.excerpts
                                    books
                                    excerpts
                                    semanticMatches
                                    model.excerptCountMap
                                    model.favCountMap
                                    query
                                ]

                        TitlePage book excerpts editMode ->
                            let
                                excerpts_ =
                                    if model.excerptSort == ExcerptFavSort then
                                        filter .isFavorite excerpts

                                    else
                                        excerpts
                            in
                            div
                                [ classList [ ( "showHints", model.showHoverUi ) ] ]
                                [ bookInfo
                                    book
                                    model.books
                                    model.tags
                                    model.pendingTag
                                    model.bookNeighborMap
                                    (getCount model.excerptCountMap book.id)
                                    (get book.id model.bookmarks)
                                    model.excerptSort
                                    progressView
                                    editMode
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
                                    model.demoMode
                                    (get book.id model.bookmarks
                                        |> withDefault ""
                                    )
                                    progressView
                                ]

                        AuthorPage author books ->
                            div []
                                [ authorInfo
                                    author
                                    books
                                    model.authorNeighborMap
                                    model.excerptCountMap
                                , bookSorter
                                    model.bookSort
                                    model.reverseSort
                                , bookList
                                    books
                                    model.excerptCountMap
                                    model.favCountMap
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
                                            (if model.demoMode then
                                                Lenses Succinct 0

                                             else
                                                Related
                                            )
                                            (get
                                                excerpt.id
                                                model.idToActiveTab
                                            )
                                        )
                                        model.demoMode
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

                        MonkPage ->
                            monkSignup model.didJoinMailingList

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
                        [ href <| repoUrl ++ "/issues"
                        , target "_blank"
                        ]
                        [ text "Report a bug" ]
                    , a
                        [ href repoUrl
                        , target "_blank"
                        ]
                        [ text "Source code" ]
                    ]
                , div [ class "fleuron" ] [ text "❦" ]
                ]
            :: (case model.modalMessage of
                    Just msgType ->
                        [ div
                            [ class "modal" ]
                            [ div
                                [ class "modalBox" ]
                                (case msgType of
                                    ErrMsg msg ->
                                        [ p
                                            []
                                            [ text "An error occurred parsing the file:" ]
                                        , div
                                            [ class "error" ]
                                            [ code [] [ text msg ] ]
                                        , actionButton
                                            [ onClick ClearModal ]
                                            [ text "Dismiss" ]
                                        ]

                                    InitErrMsg msg ->
                                        [ h4 [] [ text "This is awkward…" ]
                                        , p
                                            []
                                            [ text <|
                                                appName
                                                    ++ " uses some very new web features that your browser doesnʼt support."
                                                    ++ " Please update your browser/OS to the latest version and try again."
                                            ]
                                        , br [] []
                                        , p [] [ text "Details:" ]
                                        , div
                                            [ class "error" ]
                                            [ code [] [ text msg ] ]
                                        , actionButton
                                            [ onClick ClearModal ]
                                            [ text "Understood" ]
                                        ]

                                    InfoMsg msg ->
                                        [ p [] [ text msg ]
                                        , actionButton
                                            [ onClick ClearModal ]
                                            [ text "OK" ]
                                        ]

                                    ConfirmationMsg msg onConfirm ->
                                        [ p [] [ text msg ]
                                        , div
                                            [ class "confirm" ]
                                            [ actionButton
                                                [ class "okButton"
                                                , onClick onConfirm
                                                ]
                                                [ text "Yes, delete" ]
                                            , actionButton
                                                [ onClick ClearModal ]
                                                [ text "No, cancel" ]
                                            ]
                                        ]
                                )
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
