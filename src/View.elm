module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map)
import Model exposing (Entry, Model)
import Msg exposing (..)


view : Model -> Html Msg
view model =
    div
        [ id "container"
        , class <|
            if model.focusMode then
                "focus-mode"

            else
                ""
        ]
        [ div [ id "controls" ]
            [ input
                [ onInput OnFilter
                , id "search"
                , placeholder "search"
                , autocomplete False
                , spellcheck False
                ]
                []
            , div [ id "tools" ]
                [ span [ onClick ShowRandom ]
                    [ text "⚂" ]
                , select [ onInput FilterTitle ]
                    (option
                        [ value "*" ]
                        [ text "(all titles)" ]
                        :: map
                            (\t -> option [ value t ] [ text t ])
                            model.titles
                    )
                , span [] [ text "×" ]
                ]
            , input
                [ type_ "checkbox"
                , checked model.focusMode
                , onCheck SetFocusMode
                ]
                []
            ]
        , main_ []
            [ div [ id "sidebar" ]
                [ div []
                    [ ul []
                        (map renderEntry <|
                            if List.isEmpty model.shownEntries then
                                model.entries

                            else
                                model.shownEntries
                        )
                    ]
                ]
            , div [ id "viewer" ]
                [ case model.currentEntry of
                    Just entry ->
                        div []
                            [ p [] [ text entry.text ]
                            , div [ id "meta" ]
                                [ div
                                    [ onClick (FilterTitle entry.title)
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


charLimit =
    200


renderEntry : Entry -> Html Msg
renderEntry entry =
    li [ onClick (ShowEntry entry) ]
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
