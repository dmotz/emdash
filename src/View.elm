module View exposing (sidebarId, view)

import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy4, lazy5)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode
import List exposing (filter, length, map, member, take)
import Model exposing (Entry, Model, Tag)
import Msg exposing (..)
import Regex
import Utils exposing (formatNumber, queryCharMin)


view : Model -> Html Msg
view model =
    let
        noEntries =
            model.entries == []

        noTitleFilter =
            model.titleFilter == Nothing

        entryCount =
            length <|
                case model.shownEntries of
                    Just entries ->
                        entries

                    _ ->
                        model.entries
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
                [ div [ id "entry-count" ]
                    [ text <|
                        formatNumber entryCount
                            ++ " excerpt"
                            ++ (if entryCount == 1 then
                                    ""

                                else
                                    "s"
                               )
                    ]
                , div [ id "title-select" ]
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
                        , onClick <| FilterByTitle "*"
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
                        , onFocus <| SetInputFocus True
                        , onBlur <| SetInputFocus False
                        , id "search-input"
                        , value <| Maybe.withDefault "" model.searchFilter
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
                        , onClick <| FilterBySearch ""
                        ]
                        [ text "×" ]
                    ]
                ]
            ]
        , main_ []
            [ if noEntries then
                text ""

              else
                lazy4 sidebar
                    (case model.shownEntries of
                        Just entries ->
                            entries

                        _ ->
                            model.entries
                    )
                    model.searchFilter
                    noTitleFilter
                    model.currentEntry
            , lazy5
                viewer
                model.currentEntry
                model.parsingError
                noEntries
                model.tags
                model.pendingTag
            ]
        ]


viewer : Maybe Entry -> Bool -> Bool -> List Tag -> Maybe Tag -> Html Msg
viewer mEntry parsingError noEntries tags pendingTag =
    div [ id "viewer" ]
        [ let
            pendTag =
                Maybe.withDefault "" pendingTag
          in
          case mEntry of
            Just entry ->
                div []
                    [ blockquote [] [ text entry.text ]
                    , div [ id "meta" ]
                        [ Html.cite []
                            [ div
                                [ onClick <| FilterByTitle entry.title
                                , class "title"
                                ]
                                [ text entry.title ]
                            , div [ class "author" ] [ text entry.author ]
                            , case entry.page of
                                Just n ->
                                    div
                                        [ class "page" ]
                                        [ text <| "p. " ++ String.fromInt n ]

                                _ ->
                                    text ""
                            ]
                        , div [ class "actions" ]
                            [ div [ class "tags" ]
                                [ ul
                                    []
                                    (map
                                        (\tag ->
                                            li
                                                [ class "tag" ]
                                                [ span
                                                    [ onClick <|
                                                        FilterByTag tag
                                                    , class "tag-title"
                                                    ]
                                                    [ text tag ]
                                                , span
                                                    [ onClick <| RemoveTag tag
                                                    , class "tag-remove"
                                                    ]
                                                    [ span [] [ text "×" ] ]
                                                ]
                                        )
                                        entry.tags
                                    )
                                , span [ class "add-tag" ] [ text "add tag" ]
                                , div [ class "tag-input" ]
                                    [ input
                                        [ onInput UpdatePendingTag
                                        , onFocus <| SetInputFocus True
                                        , onBlur <| SetInputFocus False
                                        , value pendTag
                                        , placeholder "add tag"
                                        , autocomplete False
                                        , spellcheck False
                                        ]
                                        []
                                    , ul
                                        [ class "tag-list" ]
                                        (map
                                            (\tag ->
                                                li
                                                    [ onClick <| AddTag tag ]
                                                    [ text tag ]
                                            )
                                            (filter
                                                (\tag ->
                                                    member tag entry.tags
                                                        |> not
                                                        |> (&&)
                                                            (String.contains
                                                                pendTag
                                                                tag
                                                            )
                                                )
                                                tags
                                            )
                                        )
                                    ]
                                ]
                            , div
                                [ class "hide-button"
                                , onClick <| HideEntry entry
                                ]
                                [ text "×" ]
                            ]
                        ]
                    ]

            Nothing ->
                h3 [ class "intro" ]
                    [ text <|
                        if parsingError then
                            "Error parsing file."

                        else if noEntries then
                            "Drag & drop a clippings text file here. "
                                ++ "Or, click to browse."

                        else
                            "Select an entry."
                    ]
        ]


sidebarId =
    "sidebar"


sidebar : List Entry -> Maybe String -> Bool -> Maybe Entry -> Html Msg
sidebar entries query showTitles currentEntry =
    div [ id sidebarId ]
        [ if length entries == 0 then
            div [ class "no-results" ] [ text "no results" ]

          else
            Keyed.node "ul"
                []
                (map (listEntry query showTitles currentEntry) entries)
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


listEntry : Maybe String -> Bool -> Maybe Entry -> Entry -> ( String, Html Msg )
listEntry query showTitles currentEntry entry =
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
    , li [ id entry.id, onClick <| ShowEntry entry ]
        [ case currentEntry of
            Just ent ->
                if ent == entry then
                    div [ class "active-entry" ] []

                else
                    text ""

            _ ->
                text ""
        , a []
            ([ blockquote
                []
                (case query of
                    Nothing ->
                        [ text excerpt ]

                    Just q ->
                        if String.length q < queryCharMin then
                            [ text excerpt ]

                        else
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
