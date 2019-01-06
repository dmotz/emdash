port module Main exposing (main)

import Browser
import File
import List exposing (drop, filter, head, length, map, sort)
import Model exposing (Model, initialModel)
import Msg exposing (..)
import Parser
import Random exposing (generate)
import Set
import Task
import View exposing (view)


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , update = updateWithStorage
        , view =
            \m ->
                { title = "Hedera"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault initialModel maybeModel
    , Cmd.none
    )


port setStorage : Model -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        DragEnter ->
            ( model, Cmd.none )

        DragLeave ->
            ( model, Cmd.none )

        GotFiles file files ->
            ( model, Task.perform FileLoad (File.toString file) )

        FileLoad text ->
            let
                entries =
                    Parser.process text
            in
            if entries == [] then
                ( { model | parsingError = True }, Cmd.none )

            else
                ( { model
                    | parsingError = False
                    , entries = entries
                    , titles =
                        entries
                            |> map .title
                            |> Set.fromList
                            |> Set.toList
                            |> sort
                  }
                , Cmd.none
                )

        ShowEntry entry ->
            ( { model | currentEntry = Just entry }, Cmd.none )

        ShowByIndex i ->
            ( { model
                | currentEntry =
                    drop i model.entries |> head
              }
            , Cmd.none
            )

        ShowRandom ->
            ( model
            , generate
                ShowByIndex
              <|
                Random.int 0 (length model.entries - 1)
            )

        FilterBySearch rawTerm ->
            let
                term =
                    rawTerm |> String.trim |> String.toLower
            in
            if term == "" then
                ( { model
                    | shownEntries = []
                    , searchFilter = Nothing
                  }
                , Cmd.none
                )

            else
                ( { model
                    | searchFilter = Just term
                    , shownEntries =
                        filter
                            (\entry ->
                                String.contains
                                    term
                                    (String.toLower entry.text)
                            )
                            model.entries
                  }
                , Cmd.none
                )

        FilterByTitle title ->
            if title == "*" then
                ( { model
                    | shownEntries = []
                    , titleFilter = Nothing
                  }
                , Cmd.none
                )

            else
                ( { model
                    | titleFilter = Just title
                    , shownEntries =
                        filter (\entry -> entry.title == title) model.entries
                  }
                , Cmd.none
                )

        SetFocusMode active ->
            ( { model | focusMode = active }, Cmd.none )
