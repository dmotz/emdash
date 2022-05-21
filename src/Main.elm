port module Main exposing (main)

import Browser exposing (application)
import Browser.Dom exposing (getElement, setViewport)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Debounce
import Dict exposing (get, insert, values)
import Epub
import File
import File.Select as Select
import Json.Decode as Decode
import List
    exposing
        ( drop
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
import Maybe exposing (withDefault)
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
import Parser exposing (normalizeTitle)
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
        , embeddingBatchSize
        , findMatches
        , getIndex
        , getNextIndex
        , getPrevIndex
        , insertOnce
        , juxt
        , mapIdsToEntries
        , modelToStoredModel
        , queryCharMin
        , removeItem
        , updateItem
        , updateItems
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


port deleteEmbeddings : List Id -> Cmd msg


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

        bookMap =
            Parser.getBookMap restored.entries

        books =
            values bookMap

        model_ =
            { entries = restored.entries
            , idsToEntries = mapIdsToEntries restored.entries
            , neighborMap = Dict.empty
            , shownEntries = Nothing
            , hiddenEntries = Set.fromList restored.hiddenEntries
            , selectedEntries = []
            , completedEmbeddings = Set.empty
            , embeddingsReady = False
            , authors = Parser.getAuthors restored.entries
            , books = books
            , bookMap = bookMap
            , tags = Parser.getTags restored.entries
            , titleRouteMap = Parser.getRouteMap books
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
            , currentBookId = Nothing
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


getEntries : Model -> List Entry
getEntries model =
    withDefault model.entries model.shownEntries


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
            let
                ids =
                    Set.fromList <| map .id model.entries

                new =
                    Parser.process text

                entries =
                    filter
                        (\entry ->
                            (not <| Set.member entry.id model.hiddenEntries)
                                && (not <| Set.member entry.id ids)
                        )
                        new
                        ++ model.entries
            in
            if isEmpty new then
                ( { model | parsingError = True }, none )

            else
                store
                    ( { model
                        | parsingError = False
                        , entries = entries
                        , idsToEntries = mapIdsToEntries entries
                        , selectedEntries =
                            case head entries of
                                Just entry ->
                                    [ entry ]

                                _ ->
                                    []
                        , titles = Parser.getTitles entries
                        , authors = Parser.getAuthors entries
                      }
                    , none
                    )

        ResetError ->
            ( { model | parsingError = False }, none )

        ShowByIndex i ->
            case
                drop i model.entries |> head
            of
                Just entry ->
                    ( model, Nav.pushUrl model.key (entryToRoute entry) )

                _ ->
                    noOp

        ShowNext ->
            case model.selectedEntries of
                entry :: _ ->
                    update
                        (ShowByIndex <|
                            getNextIndex
                                (getEntries model)
                                entry
                        )
                        model

                [] ->
                    noOp

        ShowPrev ->
            case model.selectedEntries of
                entry :: _ ->
                    update
                        (ShowByIndex <|
                            getPrevIndex
                                (getEntries model)
                                entry
                        )
                        model

                _ ->
                    noOp

        ShowRandom ->
            ( model
            , generate
                ShowByIndex
                (Random.int 0 (length model.entries - 1))
            )

        SetInputFocus focus ->
            ( { model | inputFocused = focus }, none )

        FilterBy f ->
            let
                model_ =
                    { model | filter = f, currentBookId = Nothing }

                reset =
                    \() ->
                        update
                            (SortBooks model.bookSort)
                            { model_
                                | shownEntries = Nothing
                                , books = values model.bookMap
                            }
            in
            case f of
                Just (TitleFilter book) ->
                    let
                        entries =
                            model.entries
                                |> filter (.title >> (==) book.title)
                                |> sortBy .page
                    in
                    ( { model_
                        | shownEntries = Just entries
                        , currentBookId = Just book.id
                      }
                    , setObservers <| map .id entries
                    )

                Just (AuthorFilter author) ->
                    ( { model_
                        | shownEntries = Nothing
                        , books = filter (.author >> (==) author) model.books
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
                            | shownEntries =
                                findMatches query .text model.entries
                                    |> take maxSearchResults
                                    |> Just
                            , books =
                                findMatches
                                    query
                                    (\b -> b.title ++ " " ++ b.author)
                                    (values model.bookMap)
                          }
                        , none
                        )

                _ ->
                    reset ()

        UpdateNotes id text ->
            let
                f =
                    \entry ->
                        if entry.id == id then
                            { entry | notes = text }

                        else
                            entry
            in
            store
                ( { model
                    | entries = map f model.entries
                    , shownEntries = Maybe.map (map f) model.shownEntries
                  }
                , none
                )

        UpdatePendingTag text ->
            ( { model | pendingTag = Just text }, none )

        AddTag ->
            case model.pendingTag of
                Just tag ->
                    let
                        tagN =
                            tag |> trim |> toLower
                    in
                    if tagN == "" then
                        ( { model | pendingTag = Nothing }, none )

                    else
                        let
                            updatedSelection =
                                map
                                    (\entry ->
                                        { entry
                                            | tags =
                                                insertOnce
                                                    entry.tags
                                                    tagN
                                        }
                                    )
                                    model.selectedEntries

                            updateMapping =
                                map (juxt .id identity) updatedSelection
                                    |> Dict.fromList
                        in
                        store
                            ( { model
                                | tags = insertOnce model.tags tagN
                                , entries =
                                    updateItems
                                        model.entries
                                        updateMapping
                                , shownEntries =
                                    Maybe.map
                                        (\entries ->
                                            updateItems
                                                entries
                                                updateMapping
                                        )
                                        model.shownEntries
                                , selectedEntries = updatedSelection
                                , pendingTag = Nothing
                              }
                            , none
                            )

                _ ->
                    noOp

        RemoveTag tag ->
            let
                updatedSelection =
                    map
                        (\entry ->
                            { entry | tags = removeItem entry.tags tag }
                        )
                        model.selectedEntries

                updateMapping =
                    map (juxt .id identity) updatedSelection
                        |> Dict.fromList

                newEntries =
                    updateItems
                        model.entries
                        updateMapping
            in
            store
                ( { model
                    | entries = newEntries
                    , tags = Parser.getTags newEntries
                    , selectedEntries = updatedSelection
                    , shownEntries =
                        Maybe.map
                            (\entries -> updateItems entries updateMapping)
                            model.shownEntries
                  }
                , none
                )

        ToggleFocusMode ->
            store ( { model | focusMode = not model.focusMode }, none )

        ToggleAboutMode ->
            ( { model | aboutMode = not model.aboutMode }, none )

        PromptHide ->
            ( { model | hidePromptActive = True }, none )

        CancelHide ->
            ( { model | hidePromptActive = False }, none )

        HideEntries entries ->
            let
                list =
                    getEntries model

                idx =
                    withDefault 0 (entries |> head |> Maybe.map (getIndex list))

                fn =
                    filter (\entry -> member entry entries |> not)

                len =
                    length list

                soleEntry =
                    len == 1

                ids =
                    map .id entries

                idSet =
                    Set.fromList ids
            in
            update
                (ShowByIndex <|
                    if soleEntry then
                        0

                    else if idx == len - 1 then
                        idx - 1

                    else
                        idx
                )
                { model
                    | hiddenEntries = union model.hiddenEntries idSet
                    , entries = fn model.entries
                    , shownEntries =
                        if soleEntry then
                            Nothing

                        else
                            Maybe.map fn model.shownEntries
                    , completedEmbeddings = diff model.completedEmbeddings idSet
                    , hidePromptActive = False
                }
                |> (\( m, cmd ) -> ( m, batch [ cmd, deleteEmbeddings ids ] ))

        Sort ->
            store
                ( { model
                    | reverseSort = not model.reverseSort
                    , books = reverse model.books
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
                case key of
                    "ArrowRight" ->
                        ( model
                        , case model.currentBook of
                            Just book ->
                                case get book.id model.bookIdToLastRead of
                                    Just entryId ->
                                        case model.shownEntries of
                                            Just entries ->
                                                let
                                                    ids =
                                                        map .id entries

                                                    idx =
                                                        getIndex ids entryId
                                                in
                                                if idx == -1 || idx == (length ids - 1) then
                                                    none

                                                else
                                                    case
                                                        drop (idx + 1) ids |> head
                                                    of
                                                        Just id ->
                                                            let
                                                                _ =
                                                                    Debug.log "" id
                                                            in
                                                            attempt
                                                                ScrollToElement
                                                                (getElement <| "entry" ++ id)

                                                        _ ->
                                                            none

                                            _ ->
                                                none

                                    _ ->
                                        none

                            _ ->
                                none
                        )

                    _ ->
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
            ( model, Epub.export model.books model.entries )

        RequestEmbeddings ->
            let
                nextBatch =
                    diff
                        (diff
                            (model.entries |> map .id |> Set.fromList)
                            model.completedEmbeddings
                        )
                        model.hiddenEntries
                        |> toList
                        |> filterMap (\id -> get id model.idsToEntries)
                        |> map (\entry -> ( entry.id, entry.text ))
                        |> take embeddingBatchSize
            in
            if isEmpty nextBatch then
                ( { model | embeddingsReady = True }
                , case model.selectedEntries of
                    [ entry ] ->
                        requestNeighbors ( entry.id, True )

                    _ ->
                        none
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

        ReceiveNeighbors ( targetId, idScores ) ->
            if Dict.member targetId model.idsToEntries then
                ( { model
                    | neighborMap =
                        insert
                            targetId
                            (filterMap
                                (\( id, score ) ->
                                    case get id model.idsToEntries of
                                        Just entry ->
                                            Just ( entry, score )

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
                        case get slug model.titleRouteMap of
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
                            Just book ->
                                case get book.id model.bookIdToLastRead of
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
                        | books = sortBy .sortIndex model.books |> reverse
                        , reverseSort = False
                    }

                TitleSort ->
                    { model_
                        | books =
                            sortWith
                                (\a b ->
                                    compare
                                        (a |> .title |> normalizeTitle)
                                        (b |> .title |> normalizeTitle)
                                )
                                model.books
                        , reverseSort = True
                    }

                NumSort ->
                    { model_
                        | books = sortBy .count model.books |> reverse
                        , reverseSort = False
                    }
              -- RandomSort ->
              --     model_
            , none
            )

        OnIntersect id ->
            case get id model.idsToEntries of
                Just entry ->
                    store
                        ( case model.currentBookId of
                            Just bookId ->
                                { model
                                    | bookIdToLastRead =
                                        insert
                                            bookId
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
                | hideHeader = model.shownEntries /= Nothing && delta > 0
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
