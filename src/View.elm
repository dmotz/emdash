module View exposing (view)

import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode
import List exposing (length, map, take)
import Model exposing (Entry, Model)
import Msg exposing (..)
import Regex


view : Model -> Html Msg
view model =
    let
        noEntries =
            model.entries == []

        noTitleFilter =
            model.titleFilter == Nothing
    in
    div
        ([ id "container"
         , classList
            [ ( "focus-mode", model.focusMode )
            , ( "drag-on", model.isDragging )
            ]
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
                            , value <|
                                case model.titleFilter of
                                    Nothing ->
                                        "*"

                                    Just title ->
                                        title
                            ]
                            (option
                                [ value "*" ]
                                [ text "(all titles)" ]
                                :: map
                                    (\t -> option [ value t ] [ text t ])
                                    model.titles
                            )
                        , h5 [ classList [ ( "no-filter", noTitleFilter ) ] ]
                            [ text <|
                                case model.titleFilter of
                                    Nothing ->
                                        "(all titles)"

                                    Just title ->
                                        title
                            ]
                        ]
                    , span
                        [ classList
                            [ ( "x", True )
                            , ( "hidden", noTitleFilter )
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
                        , onFocus (SetInputFocus True)
                        , onBlur (SetInputFocus False)
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
            [ lazy3 sidebar
                (if List.isEmpty model.shownEntries then
                    model.entries

                 else
                    model.shownEntries
                )
                model.searchFilter
                noTitleFilter
            , lazy3 viewer model.currentEntry model.parsingError noEntries
            ]
        ]


viewer : Maybe Entry -> Bool -> Bool -> Html Msg
viewer mEntry parsingError noEntries =
    div [ id "viewer" ]
        [ case mEntry of
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
                h3 [ class "intro" ]
                    [ text <|
                        if parsingError then
                            "Error parsing file."

                        else if noEntries then
                            "Drag & drop a clippings txt file here (or click to browse)."

                        else
                            "Select an entry."
                    ]
        ]


sidebar : List Entry -> Maybe String -> Bool -> Html Msg
sidebar entries query showTitles =
    div [ id "sidebar" ]
        [ div []
            [ Keyed.node "ul"
                []
                (map (listEntry query showTitles) entries)
            ]
        ]


wordLimit : Int
wordLimit =
    30


addHighlighting : String -> String -> List (Html msg)
addHighlighting str query =
    let
        rx =
            Regex.fromStringWith
                { caseInsensitive = True, multiline = False }
                query
                |> Maybe.withDefault Regex.never

        addTag m =
            "<span class=\"highlight\">" ++ .match m ++ "</span>"
    in
    case Html.Parser.run <| Regex.replace rx addTag str of
        Ok parsedNodes ->
            Html.Parser.Util.toVirtualDom parsedNodes

        _ ->
            [ text str ]


listEntry : Maybe String -> Bool -> Entry -> ( String, Html Msg )
listEntry query showTitles entry =
    let
        words =
            String.words entry.text

        excerpt =
            if length words > wordLimit then
                String.join " " (take wordLimit words) ++ "…"

            else
                entry.text
    in
    ( entry.id
    , li [ onClick (ShowEntry entry) ]
        [ a []
            ([ blockquote []
                (case query of
                    Nothing ->
                        [ text excerpt ]

                    Just q ->
                        addHighlighting excerpt q
                )
             ]
                ++ (if showTitles then
                        [ Html.cite [ class "title" ] [ text entry.title ] ]

                    else
                        []
                   )
            )
        ]
    )


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
