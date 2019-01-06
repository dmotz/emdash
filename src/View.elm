module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import List exposing (map)
import Model exposing (Entry, Model)
import Msg exposing (..)


view : Model -> Html Msg
view model =
    let
        noEntries =
            model.entries == []
    in
    div
        [ id "container"
        , class <|
            if model.focusMode then
                "focus-mode"

            else
                ""
        ]
        [ div
            [ id "controls"
            , class
                (if noEntries then
                    "hidden"

                 else
                    ""
                )
            ]
            [ input
                [ onInput FilterBySearch
                , id "search"
                , placeholder "search"
                , autocomplete False
                , spellcheck False
                ]
                []
            , div [ id "tools" ]
                [ div []
                    [ div [ onClick ShowRandom, class "tool-button" ]
                        [ text "⚂" ]
                    , div [ class "tool-button" ]
                        [ text "❧"

                        -- , input
                        --    [ type_ "checkbox"
                        --    , checked model.focusMode
                        --    , onCheck SetFocusMode
                        --    ]
                        --    []
                        ]
                    ]
                , div [ id "title-select" ]
                    [ span
                        [ class
                            ("x"
                                ++ (if model.titleFilter == Nothing then
                                        " hidden"

                                    else
                                        ""
                                   )
                            )
                        , onClick (FilterByTitle "*")
                        ]
                        [ text "×" ]
                    , div []
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
                        h3 [] [ text "Select an entry" ]
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
            , div [ class "title" ] [ text entry.title ]
            ]
        ]
    )
