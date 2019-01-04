module Main exposing (main)

import Browser
import Http
import List exposing (drop, filter, head, length, map, sort)
import Model exposing (Model, initialModel)
import Msg exposing (..)
import Parser
import Random exposing (generate)
import Set
import View exposing (view)


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "exegesis"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Http.get
        { url = "/clippings.txt"
        , expect = Http.expectString OnFetch
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnFetch result ->
            case result of
                Ok text ->
                    let
                        entries =
                            Parser.process text
                    in
                    ( { model
                        | entries = entries
                        , titles =
                            entries
                                |> map .title
                                |> Set.fromList
                                |> Set.toList
                                |> sort
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

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

        OnFilter term ->
            let
                term2 =
                    term |> String.trim |> String.toLower
            in
            if term2 == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | shownEntries =
                        filter
                            (\entry ->
                                String.contains
                                    term2
                                    (String.toLower entry.text)
                            )
                            model.entries
                  }
                , Cmd.none
                )

        FilterTitle title ->
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
