module View exposing (sidebarId, view)

import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy4, lazy5)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode exposing (Decoder)
import List exposing (filter, head, length, map, member, take)
import Maybe exposing (withDefault)
import Model exposing (Entry, Filter(..), Model, Tag)
import Msg exposing (..)
import Regex
import String exposing (fromChar, slice, toList)
import Utils exposing (formatNumber, queryCharMin)


view : Model -> Html Msg
view model =
    let
        noEntries =
            model.entries == []

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
        [ header
            [ classList [ ( "hidden", noEntries ) ]
            ]
            [ div []
                [ h1 [] [ span [] [ text "❧" ], text "M" ]
                , div [ id "entry-count" ]
                    [ text <|
                        formatNumber entryCount
                            ++ " excerpt"
                            ++ (if entryCount == 1 then
                                    ""

                                else
                                    "s"
                               )
                    ]
                ]
            , div [ id "tools" ]
                [ div [ id "filters" ]
                    [ div [ id "filter-links" ]
                        (map
                            (\( mode, label ) ->
                                span
                                    [ onClick <| FilterBy mode ""
                                    , classList
                                        [ ( "active"
                                          , model.filterType == mode
                                          )
                                        ]
                                    ]
                                    [ text label ]
                            )
                            [ ( TitleFilter, "title" )
                            , ( AuthorFilter, "author" )
                            , ( TagFilter, "tag" )
                            , ( TextFilter, "text" )
                            ]
                        )
                    , div [ id "filter-controls" ]
                        [ case model.filterType of
                            TitleFilter ->
                                selectMenu
                                    model.titles
                                    model.filterValue
                                    (FilterBy TitleFilter)
                                    "titles"

                            AuthorFilter ->
                                selectMenu
                                    model.authors
                                    model.filterValue
                                    (FilterBy AuthorFilter)
                                    "authors"

                            TagFilter ->
                                selectMenu
                                    model.tags
                                    model.filterValue
                                    (FilterBy TagFilter)
                                    "tags"

                            TextFilter ->
                                div [ id "search" ]
                                    [ span
                                        [ classList
                                            [ ( "x", True )
                                            , ( "hidden", model.filterValue == Nothing )
                                            ]
                                        , onClick <| FilterBy TextFilter ""
                                        ]
                                        [ text "×" ]
                                    , input
                                        [ onInput <| FilterBy TextFilter
                                        , onFocus <| SetInputFocus True
                                        , onBlur <| SetInputFocus False
                                        , id "search-input"
                                        , value <| Maybe.withDefault "" model.filterValue
                                        , placeholder "search"
                                        , autocomplete False
                                        , spellcheck False
                                        ]
                                        []
                                    ]
                        ]
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
                    model.filterValue
                    (model.filterType
                        /= TitleFilter
                        || model.filterValue
                        == Nothing
                    )
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
                                [ onClick <| FilterBy TitleFilter entry.title
                                , class "title"
                                ]
                                [ text entry.title ]
                            , div
                                [ onClick <| FilterBy AuthorFilter entry.author
                                , class "author"
                                ]
                                [ text entry.author ]
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
                                                        FilterBy TagFilter tag
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


charLimit : Int
charLimit =
    240


takeExcerpt : String -> String
takeExcerpt text =
    let
        f acc chars n =
            case chars of
                x :: xs ->
                    if n < charLimit || n >= charLimit && x /= ' ' then
                        f (acc ++ fromChar x) xs (n + 1)

                    else
                        acc

                [] ->
                    acc
    in
    f "" (toList text) 0 ++ " …"


addHighlighting : String -> String -> List (Html msg)
addHighlighting str query =
    let
        rx =
            Regex.fromStringWith
                { caseInsensitive = True, multiline = False }
                ("\\b" ++ query)
                |> Maybe.withDefault Regex.never

        index =
            Regex.find rx str |> map .index |> head |> withDefault 0

        addTag m =
            "<span class=\"highlight\">" ++ .match m ++ "</span>"

        excerpt =
            let
                trunc =
                    if (index + String.length query) > charLimit then
                        "… " ++ slice index (String.length str) str

                    else
                        str
            in
            if String.length trunc > charLimit then
                takeExcerpt trunc

            else
                trunc
    in
    case Html.Parser.run <| Regex.replace rx addTag excerpt of
        Ok parsedNodes ->
            Html.Parser.Util.toVirtualDom parsedNodes

        _ ->
            [ text str ]


listEntry : Maybe String -> Bool -> Maybe Entry -> Entry -> ( String, Html Msg )
listEntry query showTitles currentEntry entry =
    let
        excerpt =
            if String.length entry.text > charLimit then
                takeExcerpt entry.text

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
                            addHighlighting entry.text q
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


selectMenu :
    List String
    -> Maybe String
    -> (String -> Msg)
    -> String
    -> Html Msg
selectMenu values mState inputFn default =
    let
        defaultLabel =
            "(all " ++ default ++ ")"
    in
    div [ class "select" ]
        [ span
            [ classList
                [ ( "x", True )
                , ( "hidden", mState == Nothing )
                ]
            , onClick <| inputFn ""
            ]
            [ text "×" ]
        , div [ class <| "select-" ++ default ]
            [ select
                [ onInput inputFn
                , value <|
                    case mState of
                        Just state ->
                            state

                        _ ->
                            ""
                ]
                (option
                    [ value "" ]
                    [ text defaultLabel ]
                    :: map (\t -> option [ value t ] [ text t ]) values
                )
            , h5 [ classList [ ( "no-filter", mState == Nothing ) ] ]
                [ text <|
                    case mState of
                        Just state ->
                            state

                        _ ->
                            defaultLabel
                ]
            ]
        ]


dropDecoder : Decoder Msg
dropDecoder =
    Decode.at
        [ "dataTransfer", "files" ]
        (Decode.oneOrMore GotFiles File.decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


on : String -> Decoder msg -> Attribute msg
on event decoder =
    preventDefaultOn event (Decode.map hijack decoder)
