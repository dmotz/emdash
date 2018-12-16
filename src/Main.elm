module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model exposing (Entry, Model, initialModel)
import Parser


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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnFetch result ->
            case result of
                Ok text ->
                    ( { model
                        | entries = Parser.process text
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ShowEntry entry ->
            ( { model | currentEntry = Just entry }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [ id "sidebar" ]
            [ div []
                [ ul [] (List.map renderEntry model.entries)
                ]
            ]
        , div [ id "viewer" ]
            [ case model.currentEntry of
                Just entry ->
                    div []
                        [ p [] [ text entry.text ]
                        , div [ id "meta" ]
                            [ div [ class "title" ] [ text entry.title ]
                            , div [ class "author" ] [ text entry.author ]
                            ]
                        ]

                Nothing ->
                    h3 [] [ text "Select an entry" ]
            ]
        ]


charLimit =
    200


renderEntry : Entry -> Html Msg
renderEntry entry =
    li [ onClick (ShowEntry entry) ]
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
