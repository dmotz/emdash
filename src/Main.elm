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


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [ id "sidebar" ]
            [ h1 [] [ model.entries |> List.length |> String.fromInt |> text ]
            , div []
                [ ul [] (List.map renderEntry model.entries)
                ]
            ]
        , div [ id "viewer" ] []
        ]


renderEntry : Entry -> Html Msg
renderEntry entry =
    li []
        [ blockquote [] [ text entry.text ]
        , div [] [ i [] [ text entry.title ] ]
        , div [] [ text entry.author ]
        ]
