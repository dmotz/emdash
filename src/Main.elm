module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (drop, filter, head, length, map, sort)
import Model exposing (Entry, Model, initialModel)
import Parser
import Random exposing (generate)
import Set


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


type Msg
    = OnFetch (Result Http.Error String)
    | ShowEntry Entry
    | ShowRandom
    | ShowByIndex Int
    | OnFilter String
    | FilterTitle String
    | SetFocusMode Bool


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
            ( { model | currentEntry = drop i model.entries |> head }, Cmd.none )

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
                ( { model | shownEntries = [] }, Cmd.none )

            else
                ( { model
                    | shownEntries =
                        filter (\entry -> entry.title == title) model.entries
                  }
                , Cmd.none
                )

        SetFocusMode active ->
            ( { model | focusMode = active }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ id "container"
        , class <|
            if model.focusMode then
                "focus-mode"

            else
                ""
        ]
        [ div [ id "controls" ]
            [ input
                [ onInput OnFilter
                , id "search"
                , placeholder "search"
                , autocomplete False
                , spellcheck False
                ]
                []
            , div [ id "tools" ]
                [ span [ onClick ShowRandom ]
                    [ text "⚂" ]
                , select [ onInput FilterTitle ]
                    (option
                        [ value "*" ]
                        [ text "(all titles)" ]
                        :: map
                            (\t -> option [ value t ] [ text t ])
                            model.titles
                    )
                , span [] [ text "×" ]
                ]
            , input
                [ type_ "checkbox"
                , checked model.focusMode
                , onCheck SetFocusMode
                ]
                []
            ]
        , main_ []
            [ div [ id "sidebar" ]
                [ div []
                    [ ul []
                        (map renderEntry <|
                            if List.isEmpty model.shownEntries then
                                model.entries

                            else
                                model.shownEntries
                        )
                    ]
                ]
            , div [ id "viewer" ]
                [ case model.currentEntry of
                    Just entry ->
                        div []
                            [ p [] [ text entry.text ]
                            , div [ id "meta" ]
                                [ div [ onClick (FilterTitle entry.title), class "title" ] [ text entry.title ]
                                , div [ class "author" ] [ text entry.author ]
                                ]
                            ]

                    Nothing ->
                        h3 [] [ text "Select an entry" ]
                ]
            ]
        ]


charLimit =
    200


renderEntry : Entry -> Html Msg
renderEntry entry =
    li [ onClick (ShowEntry entry) ]
        [ a []
            --[ href <| "/entry/" ++ entry.id ]
            [ blockquote []
                [ text
                    (if String.length entry.text > charLimit then
                        String.slice 0 charLimit entry.text ++ "…"

                     else
                        entry.text
                    )
                ]
            , div [ class "title" ] [ text entry.title ]
            ]
        ]
