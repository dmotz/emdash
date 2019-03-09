port module Main exposing (main)

import Browser exposing (document)
import Browser.Dom exposing (getElement, getViewportOf, setViewportOf)
import Browser.Events exposing (onKeyDown, onKeyUp)
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
        ( getIndex
        , getNextIndex
        , getPrevIndex
        , insertOnce
        , queryCharMin
        , removeItem
        , rx
        , updateItem
        )
import View exposing (sidebarId, view)


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
            let
                decoder =
                    Decode.field "key" Decode.string
            in
            \_ ->
                Sub.batch
                    [ decoder
                        |> Decode.map KeyDown
                        |> onKeyDown
                    , decoder
                        |> Decode.map KeyUp
                        |> onKeyUp
                    ]
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


port setStorage : StoredModel -> Cmd msg


store : Model -> Cmd Msg
store model =
    setStorage
        { entries = model.entries
        , currentEntry = model.currentEntry
        , hiddenEntries = Set.toList model.hiddenEntries
        , tags = model.tags
        }


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
                entries =
                    filter
                        (\entry ->
                            not <| Set.member entry.id model.hiddenEntries
                        )
                        (Parser.process text)
            in
            if entries == [] then
                ( { model | parsingError = True }, none )

            else
                let
                    newModel =
                        { model
                            | parsingError = False
                            , entries = entries
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
            ( newModel
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
                            case model.currentEntry of
                                Just entry ->
                                    if n == getIndex list entry then
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
                    applyFilter <| \entry -> entry.title == val

                AuthorFilter ->
                    applyFilter <| \entry -> entry.author == val

                TagFilter ->
                    applyFilter <| \entry -> member val entry.tags

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

        HideEntry entry ->
            let
                list =
                    withDefault model.entries model.shownEntries

                idx =
                    getIndex list entry

                fn =
                    filter ((/=) entry)
            in
            update
                (ShowByIndex <|
                    if idx == length list - 1 then
                        idx - 1

                    else
                        idx
                )
                { model
                    | hiddenEntries = insert entry.id model.hiddenEntries
                    , entries = fn model.entries
                    , shownEntries = Maybe.map fn model.shownEntries
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

        KeyDown key ->
            let
                f =
                    \m ->
                        ( { m | metaKeyCount = m.metaKeyCount + 1 }, none )
            in
            case key of
                "Meta" ->
                    f model

                "Control" ->
                    f model

                _ ->
                    if model.metaKeyCount > 0 then
                        noOp

                    else if model.inputFocused then
                        case key of
                            "Enter" ->
                                case model.pendingTag of
                                    Just tag ->
                                        update (AddTag tag) model

                                    _ ->
                                        noOp

                            _ ->
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

                            _ ->
                                noOp

        KeyUp key ->
            let
                f =
                    \m ->
                        ( { m | metaKeyCount = m.metaKeyCount - 1 }, none )
            in
            case key of
                "Meta" ->
                    f model

                "Control" ->
                    f model

                _ ->
                    noOp
