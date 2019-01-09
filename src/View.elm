module View exposing (view)

import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import List exposing (map)
import Model exposing (Entry, Model)
import Msg exposing (..)


dropDecoder : Decode.Decoder Msg
dropDecoder =
    Decode.at
        [ "dataTransfer", "files" ]
        (Decode.oneOrMore GotFiles File.decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


on : String -> Decode.Decoder msg -> Attribute msg
on event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


view : Model -> Html Msg
view model =
    let
        noEntries =
            model.entries == []
    in
    div
        ([ id "container"
         , classList [ ( "focus-mode", model.focusMode ) ]
         , on "dragenter" (Decode.succeed DragEnter)
         , on "dragover" (Decode.succeed DragEnter)
         , on "dragleave" (Decode.succeed DragLeave)
         , on "drop" dropDecoder
         ]
            ++ (if noEntries then
                    [ onClick PickFile ]

                else
                    []
               )
        )
        [ div
            [ id "controls"
            , classList [ ( "hidden", noEntries ) ]
            ]
            [ div [ id "tools" ]
                [ div [ id "title-select" ]
                    [ div []
                        [ select
                            [ onInput FilterByTitle
                            , value
                                (case model.titleFilter of
                                    Nothing ->
                                        "*"

                                    Just title ->
                                        title
                                )
                            ]
                            (option
                                [ value "*" ]
                                [ text "(all titles)" ]
                                :: map
                                    (\t -> option [ value t ] [ text t ])
                                    model.titles
                            )
                        , h5 []
                            [ text
                                (case model.titleFilter of
                                    Nothing ->
                                        "(all titles)"

                                    Just title ->
                                        title
                                )
                            ]
                        ]
                    , span
                        [ classList
                            [ ( "x", True )
                            , ( "hidden", model.titleFilter == Nothing )
                            ]
                        , onClick (FilterByTitle "*")
                        ]
                        [ text "×" ]
                    ]
                , div []
                    [ div [ onClick ShowRandom, class "tool-button" ]
                        [ text "⚂" ]
                    , div [ onClick ToggleFocusMode, class "tool-button" ]
                        [ text "❧" ]
                    ]
                , div [ id "search" ]
                    [ input
                        [ onInput FilterBySearch
                        , id "search-input"
                        , value (Maybe.withDefault "" model.searchFilter)
                        , placeholder "search"
                        , autocomplete False
                        , spellcheck False
                        ]
                        []
                    , span
                        [ classList
                            [ ( "x", True )
                            , ( "hidden", model.searchFilter == Nothing )
                            ]
                        , onClick (FilterBySearch "")
                        ]
                        [ text "×" ]
                    ]
                ]
            ]
        , main_ []
            [ lazy sidebar
                (if List.isEmpty model.shownEntries then
                    model.entries

                 else
                    model.shownEntries
                )
            , div [ id "viewer" ]
                [ case model.currentEntry of
                    Just entry ->
                        div []
                            [ p [] [ text entry.text ]
                            , div [ id "meta" ]
                                [ div
                                    [ onClick (FilterByTitle entry.title)
                                    , class "title"
                                    ]
                                    [ text entry.title ]
                                , div [ class "author" ] [ text entry.author ]
                                ]
                            ]

                    Nothing ->
                        h3 []
                            [ text
                                (if model.parsingError then
                                    "Error parsing file."

                                 else if noEntries then
                                    "Drag & drop a clippings txt file here (or click to browse)."

                                 else
                                    "Select an entry."
                                )
                            ]
                ]
            ]
        ]


sidebar : List Entry -> Html Msg
sidebar entries =
    div [ id "sidebar" ]
        [ div []
            [ Keyed.node "ul"
                []
                (map renderEntry entries)
            ]
        ]


charLimit =
    200


renderEntry : Entry -> ( String, Html Msg )
renderEntry entry =
    ( entry.id
    , li [ onClick (ShowEntry entry) ]
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
            , Html.cite [ class "title" ] [ text entry.title ]
            ]
        ]
    )
