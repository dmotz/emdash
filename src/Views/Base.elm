module Views.Base exposing (view)

import File
import Html
    exposing
        ( Html
        , button
        , div
        , h5
        , header
        , img
        , input
        , label
        , main_
        , nav
        , option
        , p
        , select
        , span
        , text
        )
import Html.Attributes
    exposing
        ( autocomplete
        , class
        , classList
        , draggable
        , id
        , placeholder
        , selected
        , spellcheck
        , src
        , value
        )
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Lazy exposing (lazy4, lazy6, lazy8)
import Json.Decode as Decode exposing (Decoder)
import List exposing (isEmpty, length, map, reverse)
import Maybe exposing (withDefault)
import Model exposing (Entry, Filter(..), InputFocus(..), Model)
import Msg exposing (Msg(..))
import Set
import Utils exposing (formatNumber, needsTitles)
import Views.About exposing (about)
import Views.Common exposing (on)
import Views.Sidebar exposing (sidebar)
import Views.Viewer exposing (viewer)


view : Model -> Html Msg
view model =
    let
        noEntries =
            isEmpty model.entries

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
        [ if model.hidePromptActive then
            hidePrompt model.selectedEntries

          else
            text ""
        , header
            []
            [ div []
                [ img
                    [ src "/logo.svg"
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
                        , span []
                            [ text <|
                                if model.reverseList then
                                    "▲"

                                else
                                    "▼"
                            ]
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
                                        , onFocus <|
                                            SetInputFocus
                                                (Just SearchFocus)
                                        , onBlur <| SetInputFocus Nothing
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
                                    [ src <| "/" ++ s ++ ".svg"
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
                        (reverse <|
                            ( "about", ToggleAboutMode )
                                :: (if noEntries then
                                        []

                                    else
                                        [ ( "random", ShowRandom )
                                        , ( "focus", ToggleFocusMode )
                                        ]
                                   )
                        )
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
                    ((if
                        model.reverseList
                            && not
                                (model.filterType
                                    == TextFilter
                                    && model.filterValue
                                    /= Nothing
                                )
                      then
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
                    model.selectedEntries
            , lazy8
                viewer
                model.selectedEntries
                model.parsingError
                noEntries
                model.tags
                model.pendingTag
                model.neighborMap
                model.embeddingsReady
                ( Set.size model.completedEmbeddings, length model.entries )
            , if model.aboutMode then
                lazy4 about model.entries model.titles model.authors model.tags

              else
                text ""
            ]
        ]


hidePrompt : List Entry -> Html Msg
hidePrompt entries =
    div [ class "prompt-bg" ]
        [ div [ class "prompt" ]
            [ p []
                [ text <|
                    "Remove "
                        ++ (if length entries == 1 then
                                "this entry?"

                            else
                                "these entries?"
                           )
                ]
            , div
                []
                [ button [ onClick <| HideEntries entries ] [ text "Yes" ]
                , button [ onClick CancelHide ] [ text "No" ]
                ]
            ]
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

        val =
            withDefault defaultLabel mState
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
            [ select [ onInput inputFn ]
                (option
                    [ value "", selected <| val == defaultLabel ]
                    [ text defaultLabel ]
                    :: map
                        (\t ->
                            option [ value t, selected <| t == val ] [ text t ]
                        )
                        values
                )
            , h5 [ classList [ ( "no-filter", mState == Nothing ) ] ]
                [ text val ]
            ]
        ]


dropDecoder : Decoder Msg
dropDecoder =
    Decode.at
        [ "dataTransfer", "files" ]
        (Decode.oneOrMore (GotFiles FileLoad) File.decoder)
