module View exposing (sidebarId, view, viewerId)

import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy4, lazy5, lazy6)
import Html.Parser
import Html.Parser.Util
import InfiniteList as IL
import Json.Decode as Decode exposing (Decoder)
import List exposing (filter, head, length, map, member, reverse, take)
import Maybe exposing (withDefault)
import Model exposing (Entry, Filter(..), Model, Tag)
import Msg exposing (..)
import Regex
import String exposing (fromChar, slice, toList)
import Utils exposing (formatNumber, getEntryHeight, needsTitles, queryCharMin)


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
                [ img
                    [ src "logo.svg"
                    , draggable "false"
                    , onClick ToggleAboutMode
                    ]
                    []
                , if noEntries then
                    text ""

                  else
                    div [ id "entry-count", onClick Sort ]
                        [ text <|
                            formatNumber entryCount
                                ++ " excerpt"
                                ++ (if entryCount == 1 then
                                        " "

                                    else
                                        "s "
                                   )
                                ++ (if model.reverseList then
                                        "▲"

                                    else
                                        "▼"
                                   )
                        ]
                ]
            , div [ id "tools" ]
                [ div [ id "filters", classList [ ( "hidden", noEntries ) ] ]
                    [ nav [ id "filter-links" ]
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
                                lazy4
                                    selectMenu
                                    model.titles
                                    model.filterValue
                                    (FilterBy TitleFilter)
                                    "titles"

                            AuthorFilter ->
                                lazy4
                                    selectMenu
                                    model.authors
                                    model.filterValue
                                    (FilterBy AuthorFilter)
                                    "authors"

                            TagFilter ->
                                lazy4
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
                lazy6
                    sidebar
                    model.infiniteList
                    model.uiSize
                    ((if model.reverseList then
                        reverse

                      else
                        identity
                     )
                        (withDefault model.entries model.shownEntries)
                    )
                    (if model.filterType == TextFilter then
                        model.filterValue

                     else
                        Nothing
                    )
                    (needsTitles model)
                    model.currentEntry
            , lazy5
                viewer
                model.currentEntry
                model.parsingError
                noEntries
                model.tags
                model.pendingTag
            , if model.aboutMode then
                aboutView

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
                            [ div [] [ text "notes:" ]
                            , textarea
                                [ onFocus <| SetInputFocus True
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
                div [ id "intro", class "info-page" ]
                    [ if parsingError then
                        p [ class "error" ] [ text "Error parsing file." ]

                      else if noEntries then
                        div []
                            [ p [ class "big" ]
                                [ text <|
                                    "This is Marginalia, a tool to "
                                        ++ "organize excerpts from ebooks "
                                        ++ "with tags, notes, and search."
                                ]
                            , h4 [] [ text "To begin:" ]
                            , div [ id "instructions" ]
                                [ p []
                                    [ text <|
                                        "Drop a clippings text file onto this "
                                            ++ "page to import its excerpts."
                                    ]
                                , p []
                                    [ text "Or, "
                                    , a [ onClick PickFile ]
                                        [ text "click here" ]
                                    , text " to browse for the file."
                                    ]
                                ]
                            , h4 [] [ em [] [ text "Nota bene:" ] ]
                            , ol []
                                [ li []
                                    [ text <|
                                        "Marginalia works entirely on your "
                                            ++ "device and stores all your "
                                            ++ "data there."
                                    ]
                                , li []
                                    [ text <|
                                        "You can easily export/import your "
                                            ++ "data (tags, notes, &c.) as "
                                    , span
                                        [ class "small-caps" ]
                                        [ text "json" ]
                                    , text "."
                                    ]
                                , li []
                                    [ text "It works offline." ]
                                , li []
                                    [ text "It’s "
                                    , a [ href repoUrl, target "_blank" ]
                                        [ text "open source" ]
                                    , text "."
                                    ]
                                , li [] [ text "You might like it." ]
                                ]
                            , p [] [ text "❦" ]
                            , footer [] [ text "Habent sua fata libelli" ]
                            ]

                      else
                        text ""
                    ]
        ]


sidebar :
    IL.Model
    -> ( Int, Int )
    -> List Entry
    -> Maybe String
    -> Bool
    -> Maybe Entry
    -> Html Msg
sidebar infiniteList ( _, h ) entries query showTitles currentEntry =
    div
        [ id sidebarId
        , classList [ ( "no-titles", not showTitles ) ]
        , IL.onScroll InfList
        ]
        [ if length entries == 0 then
            div [ class "no-results" ] [ text "no results" ]

          else
            IL.view
                (IL.config
                    { itemView = listEntry query showTitles currentEntry
                    , itemHeight =
                        IL.withConstantHeight <| getEntryHeight showTitles
                    , containerHeight = h
                    }
                    |> IL.withCustomContainer entriesContainer
                )
                infiniteList
                entries
        ]


entriesContainer : List ( String, String ) -> List (Html msg) -> Html msg
entriesContainer styles children =
    ul (map (\( k, v ) -> style k v) styles) children


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
                        "…" ++ slice index (String.length str) str

                    else
                        str
            in
            trunc
    in
    case Html.Parser.run <| Regex.replace rx addTag excerpt of
        Ok parsedNodes ->
            Html.Parser.Util.toVirtualDom parsedNodes

        _ ->
            [ text str ]


listEntry :
    Maybe String
    -> Bool
    -> Maybe Entry
    -> Int
    -> Int
    -> Entry
    -> Html Msg
listEntry query showTitles currentEntry idx listIdx entry =
    li [ id entry.id, onClick <| ShowEntry entry ]
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
                    [ text entry.text ]

                Just q ->
                    if String.length q < queryCharMin then
                        [ text entry.text ]

                    else
                        addHighlighting entry.text q
            )
        , if showTitles then
            Html.cite [ class "title" ] [ text entry.title ]

          else
            text ""
        ]


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


aboutView : Html Msg
aboutView =
    div [ id "about" ]
        [ div [ class "info-page" ]
            [ div
                [ class "hide-button", onClick ToggleAboutMode ]
                [ div [] [ text "×" ] ]
            , p [ class "big" ]
                [ text
                    "Marginalia is an open source tool created by "
                , a [ href "https://oxism.com", target "_blank" ]
                    [ text "Dan Motzenbecker" ]
                , text "."
                ]
            , h4 [] [ text "Actions" ]
            , p []
                [ a
                    [ href repoUrl
                    , target "_blank"
                    ]
                    [ text "Read the source" ]
                ]
            , p []
                [ a [ onClick ExportJson ]
                    [ text "Export "
                    , span [ class "small-caps" ] [ text "json" ]
                    ]
                ]
            , p []
                [ a [ onClick ImportJson ]
                    [ text "Import "
                    , span [ class "small-caps" ] [ text "json" ]
                    ]
                ]
            , h4 [] [ text "Colophon" ]
            , p []
                [ text "Marginalia is written in "
                , a
                    [ href "https://elm-lang.org/", target "_blank" ]
                    [ text "Elm" ]
                , text " and typeset in "
                , a
                    [ href "https://github.com/impallari/Libre-Baskerville"
                    , target "_blank"
                    ]
                    [ text "Libre Baskerville" ]
                , text "."
                ]
            , p [] [ text "❦" ]
            , footer [] [ text "Habent sua fata libelli" ]
            ]
        ]


dropDecoder : Decoder Msg
dropDecoder =
    Decode.at
        [ "dataTransfer", "files" ]
        (Decode.oneOrMore (GotFiles FileLoad) File.decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


on : String -> Decoder msg -> Attribute msg
on event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


onBlurVal : (String -> msg) -> Attribute msg
onBlurVal ev =
    Html.Events.on "blur" (Decode.map ev targetValue)


charLimit : Int
charLimit =
    42


repoUrl : String
repoUrl =
    "https://github.com/dmotz/marginalia"


viewerId : String
viewerId =
    "viewer"


sidebarId : String
sidebarId =
    "sidebar"
