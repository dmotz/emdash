module Views.TagSection exposing (tagSection)

import Html
    exposing
        ( Html
        , button
        , datalist
        , div
        , h5
        , input
        , li
        , option
        , section
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( autocomplete
        , class
        , disabled
        , id
        , list
        , placeholder
        , spellcheck
        , value
        )
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import List exposing (filter, length, map, member)
import Model exposing (Filter(..), InputFocus(..), Tag)
import Msg exposing (Msg(..))


tagSection : List Tag -> List Tag -> Maybe Tag -> Html Msg
tagSection tags globalTags pendingTag =
    let
        pendTag =
            Maybe.withDefault "" pendingTag
    in
    section []
        [ h5 [] [ text "tags:" ]
        , if length tags > 0 then
            div
                [ id "tags" ]
                [ ul
                    []
                    (map
                        (\tag ->
                            li
                                [ class "tag" ]
                                [ div
                                    [ onClick <| RemoveTag tag
                                    , class "tag-delete"
                                    ]
                                    [ text "×" ]
                                , div
                                    [ onClick <| FilterBy TagFilter tag
                                    , class "tag-title"
                                    ]
                                    [ text tag ]
                                ]
                        )
                        tags
                    )
                ]

          else
            text ""
        , let
            datalistId =
                "tags-datalist"
          in
          div [ class "tag-input" ]
            [ datalist [ id datalistId ]
                (map
                    (\tag -> option [ value tag ] [])
                    (filter
                        (\tag ->
                            member tag tags
                                |> not
                                |> (&&) (String.contains pendTag tag)
                        )
                        globalTags
                    )
                )
            , input
                [ onInput UpdatePendingTag
                , onFocus <| SetInputFocus (Just TagFocus)
                , onBlur <| SetInputFocus Nothing
                , value pendTag
                , list datalistId
                , placeholder "add tag"
                , autocomplete False
                , spellcheck False
                ]
                []
            , button
                [ onClick AddTag, disabled <| pendTag == "" ]
                [ text "Add" ]
            ]
        ]
