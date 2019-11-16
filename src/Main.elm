port module Main exposing (main)

import Browser exposing (document)
import Browser.Dom
    exposing
        ( getViewport
        , getViewportOf
        , setViewportOf
        )
import Browser.Events exposing (onKeyDown, onResize)
import Dict
import File
import File.Select as Select
import Json.Decode as Decode
import List
    exposing
        ( drop
        , filter
        , head
        , isEmpty
        , length
        , map
        , member
        , reverse
        , take
        )
import Maybe exposing (andThen, withDefault)
import Model
    exposing
        ( Filter(..)
        , Model
        , StoredModel
        , initialModel
        , initialStoredModel
        , stringToFilter
        )
import Msg exposing (..)
import Parser
import Platform.Cmd exposing (batch, none)
import Random exposing (generate)
import Regex
import Set exposing (insert)
import String exposing (toLower, trim)
import Task exposing (attempt, perform, sequence)
import Tuple exposing (first)
import Utils
    exposing
        ( KeyEvent
        , getEntryHeight
        , getIndex
        , getNextIndex
        , getPrevIndex
        , insertOnce
        , modelToStoredModel
        , needsTitles
        , queryCharMin
        , removeItem
        , rx
        , updateItem
        , updateItems
        )
import View exposing (sidebarId, view, viewerId)


port setStorage : StoredModel -> Cmd msg


port exportJson : StoredModel -> Cmd msg


port importJson : String -> Cmd msg


main : Program (Maybe StoredModel) Model Msg
main =
    document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Marginalia"
                , body = [ view m ]
                }
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onResize (\w h -> Resize ( w, h ))
                    , Decode.map3 KeyEvent
                        (Decode.field "key" Decode.string)
                        (Decode.field "ctrlKey" Decode.bool)
                        (Decode.field "metaKey" Decode.bool)
                        |> Decode.map KeyDown
                        |> onKeyDown
                    ]
        }


init : Maybe StoredModel -> ( Model, Cmd Msg )
init maybeModel =
    let
        restored =
            withDefault initialStoredModel maybeModel

        filterType =
            stringToFilter restored.filterType

        selectedIds =
            restored.entries |> map .id |> Set.fromList

        model_ =
            { initialModel
                | entries = restored.entries
                , selectedEntries =
                    filter (\entry -> Set.member entry.id selectedIds)
                        restored.entries
                , titles = Parser.getTitles restored.entries
                , authors = Parser.getAuthors restored.entries
                , tags = Parser.getTags restored.entries
                , filterType = filterType
                , focusMode = restored.focusMode
                , reverseList = restored.reverseList
            }

        model =
            case restored.filterValue of
                Just val ->
                    first <| update (FilterBy filterType val) model_

                _ ->
                    model_

        getSize =
            perform Resize
                (Task.map
                    (.viewport
                        >> (\v ->
                                ( v |> .width |> floor
                                , v |> .height |> floor
                                )
                           )
                    )
                    getViewport
                )
    in
    case restored.currentEntry of
        Just entry ->
            let
                ( m, cmd ) =
                    update (ShowEntry entry) model
            in
            ( m, batch [ getSize, cmd ] )

        _ ->
            ( model, getSize )


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
                        , currentEntry = head entries
                        , titles = Parser.getTitles entries
                        , authors = Parser.getAuthors entries
                      }
                    , none
                    )

        ResetError ->
            ( { model | parsingError = False }, none )

        ShowEntry entry ->
            let
                sidebarView =
                    getViewportOf sidebarId
            in
            store
                ( { model | currentEntry = Just entry }
                , batch
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
                    ]
                )

        ShowByIndex i ->
            case
                drop i (withDefault model.entries model.shownEntries)
                    |> head
            of
                Just entry ->
                    update (ShowEntry entry) model

                _ ->
                    noOp

        ShowNext ->
            case model.currentEntry of
                Just entry ->
                    update
                        (ShowByIndex <|
                            getNextIndex
                                (withDefault model.entries model.shownEntries)
                                entry
                        )
                        model

                _ ->
                    noOp

        ShowPrev ->
            case model.currentEntry of
                Just entry ->
                    update
                        (ShowByIndex <|
                            getPrevIndex
                                (withDefault model.entries model.shownEntries)
                                entry
                        )
                        model

                _ ->
                    noOp

        ShowRandom ->
            let
                list =
                    withDefault model.entries model.shownEntries

                len =
                    length list

                currentIndex =
                    Maybe.map (getIndex list) model.currentEntry
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

        SetInputFocus bool ->
            ( { model | inputFocused = bool, pendingTag = Nothing }, none )

        FilterBy filterType val ->
            let
                applyFilter fn =
                    if val == "" then
                        ( { model
                            | shownEntries = Nothing
                            , filterValue = Nothing
                            , filterType = filterType
                          }
                        , none
                        )

                    else
                        ( { model
                            | filterValue = Just val
                            , shownEntries = Just <| filter fn model.entries
                            , filterType = filterType
                          }
                        , none
                        )
            in
            store <|
                case filterType of
                    TitleFilter ->
                        applyFilter <| .title >> (==) val

                    AuthorFilter ->
                        applyFilter <| .author >> (==) val

                    TagFilter ->
                        applyFilter <| .tags >> member val

                    TextFilter ->
                        let
                            term =
                                toLower val
                        in
                        if trim term == "" then
                            ( { model
                                | filterValue = Nothing
                                , shownEntries = Nothing
                                , filterType = filterType
                              }
                            , none
                            )

                        else if String.length term < queryCharMin then
                            ( { model
                                | filterValue = Just val
                                , filterType = filterType
                              }
                            , none
                            )

                        else
                            applyFilter <|
                                \entry ->
                                    Regex.contains
                                        (rx <| "\\b" ++ term)
                                        (toLower entry.text)

        UpdateNotes text ->
            case model.currentEntry of
                Just entry ->
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
                            , currentEntry = Just newEntry
                            , inputFocused = False
                          }
                        , none
                        )

                _ ->
                    ( { model | inputFocused = False }, none )

        UpdatePendingTag text ->
            ( { model | pendingTag = Just text }, none )

        AddTag tag ->
            let
                tagN =
                    tag |> trim |> toLower
            in
            if tagN == "" then
                ( { model | pendingTag = Nothing }, none )

            else
                case model.currentEntry of
                    Just entry ->
                        let
                            newEntry =
                                { entry | tags = insertOnce entry.tags tagN }
                        in
                        store
                            ( { model
                                | tags = insertOnce model.tags tagN
                                , entries =
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
                                , currentEntry = Just newEntry
                                , pendingTag = Nothing
                              }
                            , none
                            )

                    _ ->
                        noOp

        RemoveTag tag ->
            case model.currentEntry of
                Just entry ->
                    let
                        newEntry =
                            { entry | tags = removeItem entry.tags tag }

                        newEntries =
                            updateItem model.entries entry newEntry
                    in
                    store
                        ( { model
                            | entries = newEntries
                            , tags = Parser.getTags newEntries
                            , currentEntry = Just newEntry
                            , shownEntries =
                                Maybe.map
                                    (\entries ->
                                        updateItem entries entry newEntry
                                    )
                                    model.shownEntries
                          }
                        , none
                        )

                _ ->
                    noOp

        ToggleFocusMode ->
            store ( { model | focusMode = not model.focusMode }, none )

        ToggleAboutMode ->
            ( { model | aboutMode = not model.aboutMode }, none )

        HideEntry entry ->
            let
                list =
                    withDefault model.entries model.shownEntries

                idx =
                    getIndex list entry

                fn =
                    filter ((/=) entry)

                len =
                    length list

                soleEntry =
                    len == 1
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
                    | hiddenEntries = insert entry.id model.hiddenEntries
                    , entries = fn model.entries
                    , shownEntries =
                        if soleEntry then
                            Nothing

                        else
                            Maybe.map fn model.shownEntries
                    , filterValue =
                        if soleEntry then
                            Nothing

                        else
                            model.filterValue
                }

        Sort ->
            store ( { model | reverseList = not model.reverseList }, none )

        GotDomEl result ->
            case result of
                Ok [ offset, height ] ->
                    case
                        model.currentEntry
                    of
                        Just entry ->
                            let
                                elHeight =
                                    needsTitles model
                                        |> getEntryHeight
                                        |> toFloat

                                targetY =
                                    getIndex
                                        ((if model.reverseList then
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

        InfList infiniteList ->
            ( { model | infiniteList = infiniteList }, none )

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

            else if model.inputFocused then
                if key == "Enter" then
                    case model.pendingTag of
                        Just tag ->
                            update (AddTag tag) model

                        _ ->
                            noOp

                else
                    noOp

            else
                case key of
                    "ArrowRight" ->
                        update
                            (if model.reverseList then
                                ShowPrev

                             else
                                ShowNext
                            )
                            model

                    "ArrowLeft" ->
                        update
                            (if model.reverseList then
                                ShowNext

                             else
                                ShowPrev
                            )
                            model

                    "r" ->
                        update ShowRandom model

                    "f" ->
                        update ToggleFocusMode model

                    "s" ->
                        update Sort model

                    "1" ->
                        update (FilterBy TitleFilter "") model

                    "2" ->
                        update (FilterBy AuthorFilter "") model

                    "3" ->
                        update (FilterBy TagFilter "") model

                    "4" ->
                        update (FilterBy TextFilter "") model

                    "Escape" ->
                        update
                            (if model.aboutMode then
                                ToggleAboutMode

                             else
                                FilterBy model.filterType ""
                            )
                            model

                    _ ->
                        noOp

        Resize size ->
            ( { model | uiSize = size }, none )
