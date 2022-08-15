port module Main exposing (main)

import Browser exposing (application)
import Browser.Dom exposing (getElement, setViewport)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Debounce
import Dict exposing (get, insert, keys, remove, values)
import Epub
import File
import File.Select as Select
import Json.Decode as Decode
import List
    exposing
        ( concat
        , concatMap
        , drop
        , filter
        , filterMap
        , head
        , isEmpty
        , length
        , map
        , member
        , reverse
        , sortBy
        , sortWith
        , take
        )
import Maybe exposing (andThen, withDefault)
import Model
    exposing
        ( BookSort(..)
        , Filter(..)
        , Id
        , InputFocus(..)
        , Model
        , StoredModel
        , TagSort(..)
        , initialStoredModel
        )
import Msg exposing (Msg(..))
import Parser exposing (normalizeTitle)
import Platform.Cmd exposing (batch, none)
import Random exposing (generate)
import Router
    exposing
        ( Route(..)
        , entryToRoute
        , routeParser
        , searchToRoute
        , slugify
        )
import Set exposing (diff, toList, union)
import String exposing (toLower, trim)
import Task exposing (attempt, perform)
import Tuple exposing (first)
import Update.Extra as Update exposing (addCmd)
import Url exposing (Url)
import Url.Parser exposing (parse)
import Utils
    exposing
        ( KeyEvent
        , dedupe
        , embeddingBatchSize
        , findMatches
        , getEntryDomId
        , getTagCounts
        , insertOnce
        , juxt
        , modelToStoredModel
        , pluckIds
        , removeItem
        , untaggedKey
        )
import Views.Base exposing (view)


port onIntersect : (Id -> msg) -> Sub msg


port onScroll : (Float -> msg) -> Sub msg


port setObservers : List Id -> Cmd msg


port setStorage : StoredModel -> Cmd msg


port scrollToTop : () -> Cmd msg


port exportJson : StoredModel -> Cmd msg


port importJson : String -> Cmd msg


port handleNewEntries : StoredModel -> Cmd msg


port requestEmbeddings : List ( Id, String ) -> Cmd msg


port receiveEmbeddings : (List Id -> msg) -> Sub msg


port requestBookEmbeddings : List ( Id, List Id ) -> Cmd msg


port receiveBookEmbeddings : (() -> msg) -> Sub msg


port deleteEmbedding : Id -> Cmd msg


port requestNeighbors : ( Id, Bool ) -> Cmd msg


port requestBookNeighbors : Id -> Cmd msg


port receiveNeighbors : (( Id, List ( Id, Float ) ) -> msg) -> Sub msg


port receiveBookNeighbors : (( Id, List ( Id, Float ) ) -> msg) -> Sub msg


port requestUnicodeNormalized : String -> Cmd msg


port receiveUnicodeNormalized : (String -> msg) -> Sub msg


appName : String
appName =
    "Marginalia"


maxSearchResults : Int
maxSearchResults =
    20


headerHeight : Float
headerHeight =
    75


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.soon 999
    , transform = DebounceMsg
    }


main : Program (Maybe StoredModel) Model Msg
main =
    application
        { init = init
        , update = update
        , view =
            \m ->
                { title =
                    (case m.filter of
                        Just filter ->
                            (case filter of
                                TitleFilter book ->
                                    book.title

                                AuthorFilter author ->
                                    author

                                TagFilter tag ->
                                    "#" ++ tag

                                TextFilter text ->
                                    "🔍 " ++ text
                            )
                                ++ " | "

                        _ ->
                            ""
                    )
                        ++ appName
                , body = [ view m ]
                }
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Decode.map3 KeyEvent
                        (Decode.field "key" Decode.string)
                        (Decode.field "ctrlKey" Decode.bool)
                        (Decode.field "metaKey" Decode.bool)
                        |> Decode.map KeyDown
                        |> onKeyDown
                    , receiveNeighbors ReceiveNeighbors
                    , receiveBookNeighbors ReceiveBookNeighbors
                    , receiveEmbeddings ReceiveEmbeddings
                    , receiveBookEmbeddings ReceiveBookEmbeddings
                    , receiveUnicodeNormalized ReceiveUnicodeNormalized
                    , onIntersect OnIntersect
                    , onScroll OnScroll
                    ]
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Maybe StoredModel -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeModel url key =
    let
        restored =
            withDefault initialStoredModel maybeModel

        books =
            Dict.fromList (map (juxt .id identity) restored.books)

        model_ =
            { entries = Dict.fromList (map (juxt .id identity) restored.entries)
            , books = books
            , booksShown = map .id restored.books
            , entriesShown = Nothing
            , neighborMap = Dict.empty
            , bookNeighborMap = Dict.empty
            , hiddenEntries = Set.fromList restored.hiddenEntries
            , completedEmbeddings = Set.empty
            , embeddingsReady = False
            , tags = restored.books |> map .tags |> concat |> dedupe
            , tagCounts = getTagCounts books
            , tagSort = TagAlphaSort
            , titleRouteMap = Parser.getTitleRouteMap restored.books
            , authorRouteMap = Parser.getAuthorRouteMap restored.books
            , notFoundMsg = Nothing
            , filter = Nothing
            , pendingTag = Nothing
            , focusMode = restored.focusMode
            , aboutMode = False
            , isDragging = False
            , reverseSort = False
            , hidePromptActive = False
            , inputFocused = Nothing
            , parsingError = False
            , schemaVersion = 0
            , url = url
            , key = key
            , bookSort = RecencySort
            , bookSortOrder = True
            , lastTitleSlug = ""
            , bookIdToLastRead = restored.bookIdToLastRead |> Dict.fromList
            , currentBook = Nothing
            , idToShowDetails = Dict.empty
            , idToActiveTab = Dict.empty
            , searchQuery = ""
            , hideHeader = False
            , searchDebounce = Debounce.init
            }
    in
    update (UrlChanged url) model_


store : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
store ( model, cmd ) =
    ( model, batch [ cmd, model |> modelToStoredModel |> setStorage ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        noOp =
            ( model, none )
    in
    case message of
        NoOp ->
            noOp

        DragEnter ->
            ( { model | isDragging = True }, none )

        DragLeave ->
            ( { model | isDragging = False }, none )

        GotFiles msg file _ ->
            ( { model | isDragging = False }
            , perform msg (File.toString file)
            )

        PickFile ->
            ( model, Select.files [ "text/plain" ] (GotFiles FileLoad) )

        FileLoad text ->
            ( model, requestUnicodeNormalized text )

        ReceiveUnicodeNormalized text ->
            let
                ( newEntries, newBooks ) =
                    Parser.process text

                allBooks =
                    Dict.union model.books newBooks

                bookVals =
                    values allBooks
            in
            if Dict.isEmpty newEntries then
                ( { model | parsingError = True }, none )

            else
                ( { model
                    | parsingError = False
                    , entries =
                        Dict.filter
                            (\id _ ->
                                not <|
                                    Set.member
                                        id
                                        model.hiddenEntries
                            )
                            (Dict.union model.entries newEntries)
                    , books = allBooks
                    , booksShown = keys allBooks
                    , titleRouteMap =
                        Parser.getTitleRouteMap bookVals
                    , authorRouteMap =
                        Parser.getAuthorRouteMap bookVals
                    , embeddingsReady = False
                  }
                , none
                )
                    |> Update.andThen update (SortBooks model.bookSort)
                    |> addCmd (model |> modelToStoredModel |> handleNewEntries)
                    |> store

        ResetError ->
            ( { model | parsingError = False }, none )

        ShowRandom ->
            ( model
            , generate
                GotRandomIndex
                (Random.int 0 ((model.entries |> values |> length) - 1))
            )

        GotRandomIndex n ->
            case model.entries |> values |> drop n |> head of
                Just entry ->
                    ( model
                    , Nav.pushUrl model.key (entryToRoute model.books entry)
                    )

                _ ->
                    noOp

        SetInputFocus focus ->
            ( { model | inputFocused = focus }, none )

        FilterBy f ->
            let
                model_ =
                    { model | filter = f, currentBook = Nothing }

                reset =
                    \() ->
                        update
                            (SortBooks model.bookSort)
                            { model_
                                | entriesShown = Nothing
                                , booksShown = keys model.books
                            }
            in
            case f of
                Just (TitleFilter book) ->
                    let
                        entryIds =
                            model.entries
                                |> Dict.filter
                                    (\_ { bookId } -> bookId == book.id)
                                |> values
                                |> sortBy .page
                                |> map .id
                    in
                    ( { model_
                        | entriesShown = Just entryIds
                        , currentBook = Just book.id
                      }
                    , setObservers entryIds
                    )

                Just (AuthorFilter author) ->
                    ( { model_
                        | entriesShown = Nothing
                        , booksShown =
                            model.books
                                |> Dict.filter
                                    (\_ book -> book.author == author)
                                |> keys
                      }
                    , none
                    )

                Just (TextFilter val) ->
                    let
                        query =
                            val |> toLower |> trim
                    in
                    if String.isEmpty query then
                        reset ()

                    else
                        ( { model_
                            | entriesShown =
                                model.entries
                                    |> values
                                    |> findMatches query .text
                                    |> take maxSearchResults
                                    |> map .id
                                    |> Just
                            , booksShown =
                                findMatches
                                    query
                                    (\b -> b.title ++ " " ++ b.author)
                                    (values model.books)
                                    |> map .id
                          }
                        , none
                        )

                Just (TagFilter tag) ->
                    ( { model_
                        | entriesShown = Nothing
                        , booksShown =
                            Dict.filter
                                (if tag == untaggedKey then
                                    \_ book -> isEmpty book.tags

                                 else
                                    \_ book -> member tag book.tags
                                )
                                model.books
                                |> keys
                      }
                    , none
                    )
                        |> Update.andThen update (SortBooks model.bookSort)

                _ ->
                    reset ()

        UpdateNotes id text ->
            store
                ( { model
                    | entries =
                        Dict.update
                            id
                            (Maybe.map (\entry -> { entry | notes = text }))
                            model.entries
                  }
                , none
                )

        UpdatePendingTag text ->
            ( { model | pendingTag = Just text }, none )

        AddTag ->
            case model.currentBook of
                Just bookId ->
                    case model.pendingTag of
                        Just tag ->
                            let
                                tagN =
                                    tag |> trim |> toLower |> slugify
                            in
                            if tagN == "" || tagN == untaggedKey then
                                ( { model | pendingTag = Nothing }, none )

                            else
                                let
                                    books =
                                        Dict.update bookId
                                            (Maybe.map
                                                (\book ->
                                                    { book
                                                        | tags =
                                                            insertOnce
                                                                book.tags
                                                                tagN
                                                    }
                                                )
                                            )
                                            model.books
                                in
                                store
                                    ( { model
                                        | books = books
                                        , tags = insertOnce model.tags tagN
                                        , tagCounts = getTagCounts books
                                        , pendingTag = Nothing
                                      }
                                    , none
                                    )

                        _ ->
                            noOp

                _ ->
                    noOp

        RemoveTag tag ->
            case model.currentBook of
                Just bookId ->
                    let
                        books =
                            Dict.update
                                bookId
                                (Maybe.map
                                    (\book ->
                                        { book
                                            | tags = removeItem book.tags tag
                                        }
                                    )
                                )
                                model.books
                    in
                    store
                        ( { model
                            | books = books
                            , tags =
                                books
                                    |> values
                                    |> concatMap .tags
                                    |> dedupe
                            , tagCounts = getTagCounts books
                          }
                        , none
                        )

                _ ->
                    noOp

        SetTagSort sort ->
            ( { model | tagSort = sort }, none )

        ToggleFocusMode ->
            store ( { model | focusMode = not model.focusMode }, none )

        ToggleAboutMode ->
            ( { model | aboutMode = not model.aboutMode }, none )

        PromptHide ->
            ( { model | hidePromptActive = True }, none )

        CancelHide ->
            ( { model | hidePromptActive = False }, none )

        HideEntry id ->
            store
                ( { model
                    | hiddenEntries = Set.insert id model.hiddenEntries
                    , entries = remove id model.entries
                    , entriesShown =
                        Maybe.map (filter ((/=) id)) model.entriesShown
                    , books =
                        withDefault
                            model.books
                            (get id model.entries
                                |> Maybe.map
                                    (\{ bookId } ->
                                        Dict.update
                                            bookId
                                            (Maybe.map
                                                (\book ->
                                                    { book
                                                        | count = book.count - 1
                                                    }
                                                )
                                            )
                                            model.books
                                    )
                            )
                  }
                , deleteEmbedding id
                )

        Sort ->
            store
                ( { model
                    | reverseSort = not model.reverseSort
                    , booksShown = reverse model.booksShown
                  }
                , none
                )

        ScrollToElement result ->
            case result of
                Ok element ->
                    ( model
                    , perform (always NoOp)
                        (setViewport 0
                            (element.element.y - headerHeight)
                        )
                    )

                Err _ ->
                    noOp

        ExportJson ->
            ( model, model |> modelToStoredModel |> exportJson )

        ImportJson ->
            ( model
            , Select.files [ "application/json" ] (GotFiles JsonFileLoad)
            )

        JsonFileLoad jsonText ->
            ( model, importJson jsonText )

        KeyDown { key, control, meta } ->
            if control || meta then
                noOp

            else if model.inputFocused /= Nothing then
                if key == "Enter" && model.inputFocused == Just TagFocus then
                    update AddTag model

                else
                    noOp

            else
                update
                    (case key of
                        "r" ->
                            ShowRandom

                        "f" ->
                            ToggleFocusMode

                        "s" ->
                            Sort

                        _ ->
                            NoOp
                    )
                    model

        ExportEpub ->
            ( model, Epub.export (values model.books) (values model.entries) )

        RequestEmbeddings ->
            let
                nextBatch =
                    diff
                        (diff
                            (model.entries |> keys |> Set.fromList)
                            model.completedEmbeddings
                        )
                        model.hiddenEntries
                        |> toList
                        |> filterMap (\id -> get id model.entries)
                        |> map (\entry -> ( entry.id, entry.text ))
                        |> take embeddingBatchSize
            in
            if isEmpty nextBatch then
                ( model
                , model.books
                    |> values
                    |> map
                        (\{ id } ->
                            ( id
                            , model.entries
                                |> values
                                |> filter (.bookId >> (==) id)
                                |> map .id
                            )
                        )
                    |> requestBookEmbeddings
                )

            else
                ( { model | embeddingsReady = False }
                , requestEmbeddings nextBatch
                )

        ReceiveEmbeddings ids ->
            update
                RequestEmbeddings
                { model
                    | completedEmbeddings =
                        union model.completedEmbeddings (Set.fromList ids)
                }

        ReceiveBookEmbeddings _ ->
            ( { model | embeddingsReady = True }
            , case model.currentBook of
                Just bookId ->
                    requestBookNeighbors bookId

                _ ->
                    none
            )

        ReceiveNeighbors ( targetId, idScores ) ->
            if Dict.member targetId model.entries then
                ( { model
                    | neighborMap =
                        insert
                            targetId
                            (filterMap
                                (\( id, score ) ->
                                    case get id model.entries of
                                        Just entry ->
                                            Just ( entry.id, score )

                                        _ ->
                                            Nothing
                                )
                                idScores
                            )
                            model.neighborMap
                  }
                , none
                )

            else
                noOp

        ReceiveBookNeighbors ( targetId, idScores ) ->
            if Dict.member targetId model.books then
                ( { model
                    | bookNeighborMap =
                        insert
                            targetId
                            (filterMap
                                (\( id, score ) ->
                                    case get id model.books of
                                        Just book ->
                                            Just ( book.id, score )

                                        _ ->
                                            Nothing
                                )
                                idScores
                            )
                            model.bookNeighborMap
                  }
                , none
                )

            else
                noOp

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if url == model.url then
                        noOp

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                model_ =
                    { model | url = url, notFoundMsg = Nothing }

                titleView =
                    \slug ->
                        case
                            get slug model.titleRouteMap
                                |> andThen (\id -> get id model.books)
                        of
                            Just book ->
                                update
                                    (FilterBy (Just (TitleFilter book)))
                                    { model_ | lastTitleSlug = slug }

                            _ ->
                                ( { model_
                                    | notFoundMsg = Just "Title not found."
                                  }
                                , none
                                )
            in
            case
                parse routeParser url
            of
                Just RootRoute ->
                    update (FilterBy Nothing) { model_ | searchQuery = "" }

                Just (TitleRoute slug) ->
                    let
                        ( m, cmd ) =
                            titleView slug
                    in
                    ( { m | searchQuery = "" }
                    , batch
                        (cmd
                            :: (case get slug model.titleRouteMap of
                                    Just bookId ->
                                        [ case
                                            get
                                                bookId
                                                model.bookIdToLastRead
                                          of
                                            Just lastId ->
                                                attempt
                                                    ScrollToElement
                                                    (lastId
                                                        |> getEntryDomId
                                                        |> getElement
                                                    )

                                            _ ->
                                                perform
                                                    (always NoOp)
                                                    (setViewport 0 0)
                                        , if model.embeddingsReady then
                                            requestBookNeighbors bookId

                                          else
                                            none
                                        ]

                                    _ ->
                                        []
                               )
                        )
                    )

                Just (EntryRoute slug id) ->
                    case get id model.entries of
                        Nothing ->
                            ( { model_
                                | notFoundMsg = Just "Excerpt ID not found."
                              }
                            , none
                            )

                        _ ->
                            let
                                ( m, cmd ) =
                                    titleView slug
                            in
                            ( { m | searchQuery = "" }
                            , batch
                                [ cmd
                                , if model.lastTitleSlug /= slug then
                                    attempt
                                        ScrollToElement
                                        (id |> getEntryDomId |> getElement)

                                  else
                                    none
                                , case
                                    m.currentBook
                                  of
                                    Just bookId ->
                                        case get bookId model.bookNeighborMap of
                                            Nothing ->
                                                if model.embeddingsReady then
                                                    requestBookNeighbors bookId

                                                else
                                                    none

                                            _ ->
                                                none

                                    _ ->
                                        none
                                ]
                            )

                Just (AuthorRoute slug) ->
                    case get slug model.authorRouteMap of
                        Just author ->
                            ( update
                                (FilterBy
                                    (Just (AuthorFilter author))
                                )
                                { model_ | searchQuery = "" }
                                |> first
                            , perform
                                (always NoOp)
                                (setViewport 0 0)
                            )

                        _ ->
                            ( { model_
                                | notFoundMsg = Just "Author not found."
                              }
                            , none
                            )

                Just (TagRoute tag) ->
                    if member tag model.tags then
                        update
                            (FilterBy (Just (TagFilter tag)))
                            { model_ | searchQuery = "" }

                    else
                        ( { model_ | notFoundMsg = Just "Tag not found." }
                        , none
                        )

                Just (TextRoute query) ->
                    case query of
                        Just text ->
                            let
                                ( debounce, cmd ) =
                                    Debounce.push
                                        debounceConfig
                                        text
                                        model.searchDebounce
                            in
                            ( { model_
                                | searchDebounce = debounce
                                , searchQuery = text
                              }
                            , cmd
                            )

                        _ ->
                            ( { model_ | searchQuery = "" }, none )

                _ ->
                    ( { model_
                        | notFoundMsg = Just "Route not found."
                        , filter = Nothing
                      }
                    , none
                    )

        SortBooks sort ->
            let
                model_ =
                    { model | bookSort = sort }
            in
            ( case sort of
                RecencySort ->
                    { model_
                        | booksShown =
                            sortBy
                                .sortIndex
                                (pluckIds model.books model.booksShown)
                                |> map .id
                                |> reverse
                        , reverseSort = False
                    }

                TitleSort ->
                    { model_
                        | booksShown =
                            sortWith
                                (\a b ->
                                    compare
                                        (a |> .title |> normalizeTitle)
                                        (b |> .title |> normalizeTitle)
                                )
                                (pluckIds model.books model.booksShown)
                                |> map .id
                        , reverseSort = True
                    }

                NumSort ->
                    { model_
                        | booksShown =
                            sortBy
                                .count
                                (pluckIds model.books model.booksShown)
                                |> map .id
                                |> reverse
                        , reverseSort = False
                    }
            , none
            )

        OnIntersect id ->
            case get id model.entries of
                Just entry ->
                    store
                        ( case
                            model.currentBook
                                |> andThen (\bookId -> get bookId model.books)
                          of
                            Just book ->
                                { model
                                    | bookIdToLastRead =
                                        insert
                                            book.id
                                            id
                                            model.bookIdToLastRead
                                }

                            _ ->
                                model
                        , Nav.replaceUrl
                            model.key
                            (entryToRoute model.books entry)
                        )

                _ ->
                    noOp

        ToggleDetails id ->
            let
                newState =
                    get id model.idToShowDetails |> withDefault False |> not
            in
            ( { model
                | idToShowDetails = insert id newState model.idToShowDetails
              }
            , if newState && get id model.neighborMap == Nothing then
                requestNeighbors ( id, True )

              else
                none
            )

        SetEntryTab id tab ->
            ( { model
                | idToActiveTab = insert id tab model.idToActiveTab
              }
            , none
            )

        ScrollToTop ->
            ( model, scrollToTop () )

        OnSearch query ->
            if String.isEmpty query then
                ( { model | searchQuery = "" }, Nav.replaceUrl model.key "/" )

            else
                ( model, Nav.replaceUrl model.key (searchToRoute query) )

        OnScroll delta ->
            ( { model
                | hideHeader = model.entriesShown /= Nothing && delta > 0
              }
            , none
            )

        DebounceMsg msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast
                            (\text ->
                                Task.perform
                                    (\t -> FilterBy (Just (TextFilter t)))
                                    (Task.succeed text)
                            )
                        )
                        msg
                        model.searchDebounce
            in
            ( { model | searchDebounce = debounce }
            , cmd
            )
