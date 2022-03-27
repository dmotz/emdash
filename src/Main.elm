port module Main exposing (main)

import Browser exposing (application)
import Browser.Dom
    exposing
        ( getViewport
        , getViewportOf
        , setViewportOf
        )
import Browser.Events exposing (onKeyDown, onResize)
import Browser.Navigation as Nav
import Dict exposing (get, values)
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
        , maximum
        , member
        , minimum
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
import Parser
import Platform.Cmd exposing (batch, none)
import Random exposing (generate)
import Regex
import Router exposing (Route(..), deslugify, entryToRoute, routeParser)
import Set exposing (diff, toList, union)
import String exposing (join, split, toLower, trim)
import Task exposing (attempt, perform, sequence)
import Tuple exposing (first)
import Url exposing (Url)
import Url.Parser exposing (parse)
import Utils
    exposing
        ( KeyEvent
        , embeddingBatchSize
        , getEntryHeight
        , getIndex
        , getNextIndex
        , getPrevIndex
        , insertOnce
        , mapIdsToEntries
        , modelToStoredModel
        , queryCharMin
        , removeItem
        , rx
        , updateItem
        , updateItems
        )
import Views.Base exposing (view)
import Views.Sidebar exposing (sidebarId)
import Views.Viewer exposing (viewerId)


port setStorage : StoredModel -> Cmd msg


port exportJson : StoredModel -> Cmd msg


port importJson : String -> Cmd msg


port requestEmbeddings : List ( Id, String ) -> Cmd msg


port receiveEmbeddings : (List Id -> msg) -> Sub msg


port deleteEmbeddings : List Id -> Cmd msg


port requestNeighbors : ( Id, Bool ) -> Cmd msg


port receiveNeighbors : (( Id, List ( Id, Float ) ) -> msg) -> Sub msg


appName : String
appName =
    "Marginalia"


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
                    ]
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Maybe StoredModel -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeModel url key =
    let
        restored =
            withDefault initialStoredModel maybeModel

        titles =
            Parser.getTitles restored.entries

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
            , titles = titles
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
            }

        model =
            update (UrlChanged url) model_ |> first
    in
    ( model, none )


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

        SelectEntries entries ->
            store
                ( { model
                    | selectedEntries = entries
                    , hidePromptActive = False
                  }
                , if length entries == 1 then
                    let
                        sidebarView =
                            getViewportOf sidebarId
                    in
                    batch
                        [ attempt
                            GotDomEl
                            (sequence
                                [ Task.map (.viewport >> .y) sidebarView
                                , Task.map (.viewport >> .height) sidebarView
                                ]
                            )
                        , attempt
                            DidScroll
                            (setViewportOf viewerId 0 0)
                        , case head entries of
                            Just entry ->
                                if
                                    model.embeddingsReady
                                        && get entry.id model.neighborMap
                                        == Nothing
                                then
                                    requestNeighbors ( entry.id, True )

                                else
                                    none

                            Nothing ->
                                none
                        ]

                  else
                    none
                )

        EntryClick entry { control, meta, shift } ->
            if shift then
                let
                    entries =
                        (if model.reverseList then
                            reverse

                         else
                            identity
                        )
                        <|
                            getEntries model

                    selectedIndices =
                        map (getIndex entries) model.selectedEntries

                    targetIndex =
                        getIndex entries entry

                    minIndex =
                        withDefault 0 (minimum selectedIndices)

                    maxIndex =
                        withDefault 0 (maximum selectedIndices)

                    start =
                        if targetIndex < minIndex then
                            targetIndex

                        else
                            minIndex

                    end =
                        if targetIndex < minIndex then
                            maxIndex

                        else
                            targetIndex
                in
                update
                    (SelectEntries
                        (entries
                            |> drop start
                            |> take (end - start + 1)
                        )
                    )
                    model

            else if control || meta then
                update
                    (SelectEntries <|
                        (if member entry model.selectedEntries then
                            filter ((/=) entry)

                         else
                            (::) entry
                        )
                            model.selectedEntries
                    )
                    model

            else
                noOp

        ShowByIndex i ->
            case
                drop i (getEntries model) |> head
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
            let
                list =
                    getEntries model

                len =
                    length list

                currentIndex =
                    model.selectedEntries
                        |> head
                        |> andThen (getIndex list >> Just)
            in
            case len of
                0 ->
                    noOp

                1 ->
                    noOp

                2 ->
                    update ShowNext model

                _ ->
                    ( model
                    , generate
                        (\n ->
                            case currentIndex of
                                Just index ->
                                    if n == index then
                                        ShowRandom

                                    else
                                        ShowByIndex n

                                _ ->
                                    ShowByIndex n
                        )
                        (Random.int 0 (len - 1))
                    )

        SetInputFocus focus ->
            ( { model | inputFocused = focus }, none )

        FilterBy f ->
            let
                model_ =
                    { model | filter = f }
            in
            case f of
                Just (TitleFilter book) ->
                    ( { model_
                        | shownEntries =
                            Just <|
                                filter
                                    (.title >> (==) book.title)
                                    model.entries
                      }
                    , none
                    )

                Just (AuthorFilter author) ->
                    ( { model_
                        | shownEntries = Nothing
                        , books = filter (.author >> (==) author) model.books
                      }
                    , none
                    )

                _ ->
                    update
                        (SortBooks model.bookSort)
                        { model_
                            | shownEntries = Nothing
                            , books = values model.bookMap
                        }

        UpdateNotes text ->
            case model.selectedEntries of
                [ entry ] ->
                    let
                        newEntry =
                            { entry | notes = text }
                    in
                    store
                        ( { model
                            | entries =
                                updateItem
                                    model.entries
                                    entry
                                    newEntry
                            , shownEntries =
                                Maybe.map
                                    (\entries ->
                                        updateItem
                                            entries
                                            entry
                                            newEntry
                                    )
                                    model.shownEntries
                            , selectedEntries = [ newEntry ]
                            , inputFocused = Nothing
                          }
                        , none
                        )

                _ ->
                    ( { model | inputFocused = Nothing }, none )

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
                                map
                                    (\entry -> ( entry.id, entry ))
                                    updatedSelection
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
                    map (\entry -> ( entry.id, entry )) updatedSelection
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
                    , filterValue =
                        if soleEntry then
                            Nothing

                        else
                            model.filterValue
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

        GotDomEl result ->
            case result of
                Ok [ offset, height ] ->
                    case
                        model.selectedEntries
                    of
                        entry :: _ ->
                            let
                                elHeight =
                                    needsTitles model
                                        |> getEntryHeight
                                        |> toFloat

                                targetY =
                                    getIndex
                                        ((if model.reverseSort then
                                            reverse

                                          else
                                            identity
                                         )
                                            (withDefault
                                                model.entries
                                                model.shownEntries
                                            )
                                        )
                                        entry
                                        |> toFloat
                                        |> (*) elHeight
                            in
                            if
                                targetY
                                    + elHeight
                                    > (offset + height)
                                    || targetY
                                    < offset
                            then
                                ( model
                                , attempt
                                    DidScroll
                                    (setViewportOf sidebarId 0 targetY)
                                )

                            else
                                noOp

                        _ ->
                            noOp

                Ok _ ->
                    noOp

                Err _ ->
                    noOp

        DidScroll _ ->
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
            if (model.inputFocused == Nothing) && (control || meta) then
                case key of
                    "a" ->
                        update
                            (SelectEntries <|
                                withDefault
                                    model.entries
                                    model.shownEntries
                            )
                            model

                    _ ->
                        noOp

            else if model.inputFocused /= Nothing then
                if key == "Enter" && model.inputFocused == Just TagFocus then
                    update AddTag model

                else
                    noOp

            else
                update
                    (case key of
                        "ArrowRight" ->
                            if model.reverseList then
                                ShowPrev

                            else
                                ShowNext

                        "ArrowLeft" ->
                            if model.reverseList then
                                ShowNext

                            else
                                ShowPrev

                        "r" ->
                            ShowRandom

                        "f" ->
                            ToggleFocusMode

                        "s" ->
                            Sort

                        "1" ->
                            FilterBy TitleFilter ""

                        "2" ->
                            FilterBy AuthorFilter ""

                        "3" ->
                            FilterBy TagFilter ""

                        "4" ->
                            FilterBy TextFilter ""

                        "Escape" ->
                            if model.aboutMode then
                                ToggleAboutMode

                            else
                                FilterBy model.filterType ""

                        _ ->
                            NoOp
                    )
                    model

        ExportEpub ->
            ( model, Epub.export model.titles model.entries )

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
                        Dict.insert
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
            in
            case
                parse routeParser url
            of
                Just RootRoute ->
                    update (FilterBy Nothing) model_

                Just (TitleRoute slug) ->
                    case get slug model.titleRouteMap of
                        Just book ->
                            update (FilterBy (Just (TitleFilter book))) model_

                        _ ->
                            noOp_

                Just (AuthorRoute author) ->
                    update (FilterBy (Just (AuthorFilter (deslugify author)))) model_

                Just (EntryRoute _ id) ->
                    case
                        get id model.idsToEntries
                    of
                        Just entry ->
                            update (SelectEntries [ entry ]) model_

                        _ ->
                            noOp_

                Just (TagRoute tag) ->
                    update (FilterBy (Just (TagFilter (deslugify tag)))) model_

                Just (TextRoute query) ->
                    case query of
                        Just text ->
                            update (FilterBy (Just (TextFilter text))) model_

                        _ ->
                            noOp_

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
                        | books = sortBy .title model.books
                        , reverseSort = True
                    }

                NumSort ->
                    { model_
                        | books = sortBy .count model.books |> reverse
                        , reverseSort = False
                    }
            , none
            )
