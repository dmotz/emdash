port module Main exposing (main)

import Browser exposing (application)
import Browser.Dom exposing (getElement, setViewport)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Debounce
import Dict exposing (get, insert, keys, values)
import Epub
import File
import File.Select as Select
import Json.Decode as Decode
import List
    exposing
        ( concatMap
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
        , Entry
        , Filter(..)
        , Id
        , InputFocus(..)
        , Model
        , StoredModel
        , initialStoredModel
        )
import Msg exposing (Msg(..))
import Parser exposing (getBooks, normalizeTitle)
import Platform.Cmd exposing (batch, none)
import Random exposing (generate)
import Router
    exposing
        ( Route(..)
        , deslugify
        , entryToRoute
        , routeParser
        , searchToRoute
        )
import Set exposing (diff, toList, union)
import String exposing (toLower, trim)
import Task exposing (attempt, perform)
import Tuple exposing (first)
import Url exposing (Url)
import Url.Parser exposing (parse)
import Utils
    exposing
        ( KeyEvent
        , dedupe
        , embeddingBatchSize
        , findMatches
        , getIndex
        , getNextIndex
        , getPrevIndex
        , insertOnce
        , juxt
        , modelToStoredModel
        , pluckIds
        , removeItem
        , updateItem
        )
import Views.Base exposing (view)


port onIntersect : (Id -> msg) -> Sub msg


port onScroll : (Float -> msg) -> Sub msg


port setObservers : List Id -> Cmd msg


port setStorage : StoredModel -> Cmd msg


port scrollToTop : () -> Cmd msg


port exportJson : StoredModel -> Cmd msg


port importJson : String -> Cmd msg


port requestEmbeddings : List ( Id, String ) -> Cmd msg


port receiveEmbeddings : (List Id -> msg) -> Sub msg


port deleteEmbedding : Id -> Cmd msg


port requestNeighbors : ( Id, Bool ) -> Cmd msg


port receiveNeighbors : (( Id, List ( Id, Float ) ) -> msg) -> Sub msg


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
                            case filter of
                                TitleFilter book ->
                                    book.title ++ " - "

                                _ ->
                                    ""

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
                    , receiveEmbeddings ReceiveEmbeddings
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

        model_ =
            { entries = Dict.fromList (map (juxt .id identity) restored.entries)
            , books = Dict.fromList (map (juxt .id identity) restored.books)
            , booksShown = map .id restored.books
            , entriesShown = Nothing
            , neighborMap = Dict.empty
            , hiddenEntries = Set.fromList restored.hiddenEntries
            , completedEmbeddings = Set.empty
            , embeddingsReady = False
            , tags = Parser.getTags restored.entries
            , titleRouteMap = Parser.getRouteMap restored.books
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
                newEntries =
                    Parser.process text

                entries =
                    Dict.filter
                        (\id _ -> not <| Set.member id model.hiddenEntries)
                        (Dict.union model.entries newEntries)

                books =
                    entries |> values |> getBooks
            in
            if Dict.isEmpty newEntries then
                ( { model | parsingError = True }, none )

            else
                store <|
                    update
                        (SortBooks model.bookSort)
                        { model
                            | parsingError = False
                            , entries = entries
                            , books = books
                            , booksShown = keys books
                        }

        ResetError ->
            ( { model | parsingError = False }, none )

        ShowRandom ->
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
                                    (\_ { title } -> title == book.title)
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
                            Dict.filter
                                (\_ entry -> entry.author == author)
                                model.books
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
                                findMatches query .text (values model.entries)
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
                                (\_ book -> member tag book.tags)
                                model.books
                                |> keys
                      }
                    , none
                    )

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
                                    tag |> trim |> toLower
                            in
                            if tagN == "" then
                                ( { model | pendingTag = Nothing }, none )

                            else
                                store
                                    ( { model
                                        | tags = insertOnce model.tags tagN
                                        , books =
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
                        newBooks =
                            Dict.update bookId
                                (Maybe.map
                                    (\book ->
                                        { book
                                            | tags =
                                                removeItem book.tags tag
                                        }
                                    )
                                )
                                model.books
                    in
                    store
                        ( { model
                            | tags =
                                newBooks
                                    |> values
                                    |> concatMap .tags
                                    |> dedupe
                            , books = newBooks
                          }
                        , none
                        )

                _ ->
                    noOp

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
                    , entriesShown =
                        Maybe.map (filter ((/=) id)) model.entriesShown
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
                ( { model | embeddingsReady = True }, none )

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
                    { model | url = url }

                noOp_ =
                    ( model_, none )

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
                                noOp_
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
                        [ cmd
                        , case get slug model.titleRouteMap of
                            Just bookId ->
                                case get bookId model.bookIdToLastRead of
                                    Just lastId ->
                                        attempt
                                            ScrollToElement
                                            (getElement <| "entry" ++ lastId)

                                    _ ->
                                        perform (always NoOp) (setViewport 0 0)

                            _ ->
                                none
                        ]
                    )

                Just (EntryRoute slug id) ->
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
                                (getElement <| "entry" ++ id)

                          else
                            none
                        ]
                    )

                Just (AuthorRoute author) ->
                    ( update
                        (FilterBy
                            (Just (AuthorFilter (deslugify author)))
                        )
                        { model_ | searchQuery = "" }
                        |> first
                    , perform
                        (always NoOp)
                        (setViewport 0 0)
                    )

                Just (TagRoute tag) ->
                    update
                        (FilterBy (Just (TagFilter (deslugify tag))))
                        { model_ | searchQuery = "" }

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
                            ( { model
                                | searchDebounce = debounce
                                , searchQuery = text
                              }
                            , cmd
                            )

                        _ ->
                            ( { model_ | searchQuery = "" }, none )

                _ ->
                    noOp_

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
                        , Nav.replaceUrl model.key (entryToRoute entry)
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
