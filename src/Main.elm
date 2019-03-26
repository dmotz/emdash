port module Main exposing (main)

import Browser exposing (document)
import Browser.Dom exposing (getElement, getViewportOf, setViewportOf)
import Browser.Events exposing (onKeyDown)
import File
import File.Select as Select
import Json.Decode as Decode
import List exposing (drop, filter, head, isEmpty, length, map, member)
import Maybe exposing (withDefault)
import Model
    exposing
        ( Filter(..)
        , Model
        , StoredModel
        , initialModel
        , initialStoredModel
        )
import Msg exposing (..)
import Parser
import Platform.Cmd exposing (batch, none)
import Random exposing (generate)
import Regex
import Set exposing (insert)
import String exposing (toLower, trim)
import Task exposing (attempt, sequence)
import Utils
    exposing
        ( KeyEvent
        , getIndex
        , getNextIndex
        , getPrevIndex
        , insertOnce
        , modelToStoredModel
        , queryCharMin
        , removeItem
        , rx
        , updateItem
        )
import View exposing (sidebarId, view)


port setStorage : StoredModel -> Cmd msg


port exportJson : StoredModel -> Cmd msg


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
                Decode.map3 KeyEvent
                    (Decode.field "key" Decode.string)
                    (Decode.field "ctrlKey" Decode.bool)
                    (Decode.field "metaKey" Decode.bool)
                    |> Decode.map KeyDown
                    |> onKeyDown
        }


init : Maybe StoredModel -> ( Model, Cmd Msg )
init maybeModel =
    let
        restored =
            withDefault initialStoredModel maybeModel

        model =
            { initialModel
                | entries = restored.entries
                , currentEntry = restored.currentEntry
                , titles = Parser.getTitles restored.entries
                , authors = Parser.getAuthors restored.entries
                , tags = Parser.getTags restored.entries
            }
    in
    case restored.currentEntry of
        Just entry ->
            update (ShowEntry entry) model

        _ ->
            ( model, none )


store : Model -> Cmd Msg
store =
    modelToStoredModel >> setStorage


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

        GotFiles file _ ->
            ( { model | isDragging = False }
            , Task.perform FileLoad (File.toString file)
            )

        PickFile ->
            ( model, Select.files [ "text/plain" ] GotFiles )

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
            if new == [] then
                ( { model | parsingError = True }, none )

            else
                let
                    newModel =
                        { model
                            | parsingError = False
                            , entries = entries
                            , currentEntry = head entries
                            , titles = Parser.getTitles entries
                            , authors = Parser.getAuthors entries
                        }
                in
                ( newModel, store newModel )

        ShowEntry entry ->
            let
                newModel =
                    { model | currentEntry = Just entry }

                sidebarView =
                    getViewportOf sidebarId

                entryElement =
                    getElement entry.id
            in
            ( { newModel | editMode = False }
            , batch
                [ attempt
                    GotDomEl
                    (sequence
                        [ Task.map (.viewport >> .y) sidebarView
                        , Task.map (.viewport >> .height) sidebarView
                        , Task.map (.element >> .y) (getElement sidebarId)
                        , Task.map (.element >> .y) entryElement
                        , Task.map (.element >> .height) entryElement
                        ]
                    )
                , store newModel
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
                        ( { model | filterValue = Just val, filterType = filterType }, none )

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

                        newModel =
                            { model
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
                            }
                    in
                    ( newModel, store newModel )

                _ ->
                    noOp

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

                            newModel =
                                { model
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
                        in
                        ( newModel, store newModel )

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

                        newModel =
                            { model
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
                    in
                    ( newModel, store newModel )

                _ ->
                    noOp

        ToggleFocusMode ->
            ( { model | focusMode = not model.focusMode }, none )

        ToggleAboutMode ->
            ( { model | aboutMode = not model.aboutMode }, none )

        EnterEditMode ->
            ( { model | editMode = True }, none )

        ExitEditMode ->
            ( { model | editMode = False }, none )

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

        GotDomEl result ->
            case result of
                Ok [ offset, height, parentY, childY, elHeight ] ->
                    let
                        targetY =
                            childY - parentY
                    in
                    if targetY + elHeight > height || targetY < 0 then
                        ( model
                        , attempt
                            DidScroll
                            (setViewportOf
                                sidebarId
                                0
                                (offset + targetY)
                            )
                        )

                    else
                        noOp

                Ok _ ->
                    noOp

                Err _ ->
                    noOp

        DidScroll _ ->
            noOp

        ExportJson ->
            ( model, model |> modelToStoredModel |> exportJson )

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
                        update ShowNext model

                    "ArrowLeft" ->
                        update ShowPrev model

                    "r" ->
                        update ShowRandom model

                    "f" ->
                        update ToggleFocusMode model

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
