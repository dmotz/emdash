module View exposing (sidebarId, view, viewerId)

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
        [ id "container"
        , classList
            [ ( "focus-mode", model.focusMode )
            , ( "drag-on", model.isDragging )
            , ( "empty", noEntries )
            ]
        , on "dragenter" (Decode.succeed DragEnter)
        , on "dragover" (Decode.succeed DragEnter)
        , on "dragleave" (Decode.succeed DragLeave)
        , on "drop" dropDecoder
        ]
        [ header
            []
            [ div []
                [ img [ src "logo.svg", draggable "false" ] []
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
                [ div [ id "filters", classList [ ( "hidden", noEntries ) ] ]
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
                                            , ( "hidden"
                                              , model.filterValue == Nothing
                                              )
                                            ]
                                        , onClick <| FilterBy TextFilter ""
                                        ]
                                        [ text "×" ]
                                    , input
                                        [ onInput <| FilterBy TextFilter
                                        , onFocus <| SetInputFocus True
                                        , onBlur <| SetInputFocus False
                                        , id "search-input"
                                        , value <|
                                            Maybe.withDefault
                                                ""
                                                model.filterValue
                                        , placeholder "search"
                                        , autocomplete False
                                        , spellcheck False
                                        ]
                                        []
                                    ]
                        ]
                    ]
                , div [ id "actions" ]
                    (map
                        (\( s, action ) ->
                            div [ onClick action ]
                                [ img
                                    [ src <| s ++ ".svg"
                                    , draggable "false"
                                    ]
                                    []
                                , label []
                                    [ text <|
                                        if s == "about" then
                                            "&c."

                                        else
                                            s
                                    ]
                                ]
                        )
                        [ ( "focus", ToggleFocusMode )
                        , ( "random", ShowRandom )
                        , ( "about", ToggleAboutMode )
                        ]
                    )
                ]
            ]
        , main_ []
            [ if noEntries then
                text ""

              else
                lazy4
                    sidebar
                    (withDefault model.entries model.shownEntries)
                    (if model.filterType == TextFilter then
                        model.filterValue

                     else
                        Nothing
                    )
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
            , if model.aboutMode then
                div [ id "about" ]
                    [ div [ onClick ToggleAboutMode, class "x" ] [ text "×" ]
                    , h1 [] [ text "Marginalia" ]
                    , p []
                        [ text "Marginalia is an open source tool created by "
                        , a [ href "https://oxism.com", target "_blank" ]
                            [ text "Dan Motzenbecker" ]
                        , text "."
                        ]
                    , p []
                        [ a
                            [ href repoUrl
                            , target "_blank"
                            ]
                            [ text "View source" ]
                        ]
                    , p [] [ a [ onClick ExportJson ] [ text "Export JSON" ] ]
                    ]

              else
                text ""
            ]
        ]


viewer :
    Maybe Entry
    -> Bool
    -> Bool
    -> List Tag
    -> Maybe Tag
    -> Html Msg
viewer mEntry parsingError noEntries tags pendingTag =
    div
        ([ id "viewer" ]
            ++ (if parsingError then
                    [ onClick ResetError ]

                else
                    []
               )
        )
        [ let
            pendTag =
                Maybe.withDefault "" pendingTag
          in
          case mEntry of
            Just entry ->
                div []
                    [ blockquote [] [ text entry.text ]
                    , Html.cite [ id "meta" ]
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
                    , div
                        [ id "entry-tools" ]
                        [ section []
                            [ if length entry.tags > 0 then
                                div
                                    [ id "tags" ]
                                    [ ul
                                        []
                                        (map
                                            (\tag ->
                                                li
                                                    [ class "tag" ]
                                                    [ span
                                                        [ onClick <|
                                                            RemoveTag tag
                                                        , class "x"
                                                        ]
                                                        [ text "×" ]
                                                    , span
                                                        [ onClick <|
                                                            FilterBy
                                                                TagFilter
                                                                tag
                                                        , class "tag-title"
                                                        ]
                                                        [ text tag ]
                                                    ]
                                            )
                                            entry.tags
                                        )
                                    ]

                              else
                                text ""
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
                                , let
                                    tagList =
                                        filter
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
                                  in
                                  if length tagList > 0 then
                                    ul
                                        [ class "tag-list" ]
                                        (map
                                            (\tag ->
                                                li
                                                    [ onClick <|
                                                        AddTag tag
                                                    ]
                                                    [ text tag ]
                                            )
                                            tagList
                                        )

                                  else
                                    text ""
                                ]
                            ]
                        , section []
                            [ textarea
                                [ placeholder "notes"
                                , onFocus <| SetInputFocus True
                                , onBlurVal UpdateNotes
                                , value entry.notes
                                ]
                                [ text entry.notes
                                ]
                            ]
                        , section []
                            [ div
                                [ class "hide-button"
                                , onClick <| HideEntry entry
                                ]
                                [ div [] [ text "×" ]
                                , span [] [ text "delete entry" ]
                                ]
                            ]
                        ]
                    ]

            Nothing ->
                div [ id "intro" ]
                    [ if parsingError then
                        p [] [ text "Error parsing file." ]

                      else if noEntries then
                        div []
                            [ p []
                                [ text <|
                                    "This is Marginalia, a tool to "
                                        ++ "organize excerpts from e-books "
                                        ++ "with tags, notes, and filtering."
                                ]
                            , p []
                                [ text <|
                                    "Drop a clippings text file onto the page "
                                        ++ "to import it."
                                ]
                            , p []
                                [ text "Or, "
                                , a [ onClick PickFile ] [ text "click here" ]
                                , text " to browse for the file."
                                ]
                            , p [] [ text "❦" ]
                            , ol []
                                [ li []
                                    [ text <|
                                        "Marginalia works entirely on your "
                                            ++ "device and stores all your "
                                            ++ "data there."
                                    ]
                                , li []
                                    [ text <|
                                        "You can easily export your data "
                                            ++ "(tags, notes, etc.) as JSON."
                                    ]
                                , li []
                                    [ text "It works offline." ]
                                , li []
                                    [ text "It’s "
                                    , a [ href repoUrl, target "_blank" ]
                                        [ text "open source" ]
                                    , text "."
                                    ]
                                ]
                            , footer [] [ text "Habent sua fata libelli" ]
                            ]

                      else
                        text ""
                    ]
        ]


repoUrl : String
repoUrl =
    "https://github.com/dmotz/marginalia"


viewerId : String
viewerId =
    "viewer"


sidebarId : String
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
        , blockquote
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
        , if showTitles then
            Html.cite [ class "title" ] [ text entry.title ]

          else
            text ""
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


onBlurVal : (String -> msg) -> Attribute msg
onBlurVal ev =
    Html.Events.on "blur" (Decode.map ev targetValue)
