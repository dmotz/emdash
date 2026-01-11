module Views.TagSection exposing (tagSection)

import Html
    exposing
        ( Html
        , a
        , button
        , datalist
        , div
        , form
        , h5
        , input
        , li
        , option
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( attribute
        , autocomplete
        , class
        , disabled
        , href
        , id
        , list
        , placeholder
        , spellcheck
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import List exposing (filter, isEmpty, map, member)
import Msg exposing (Msg(..))
import Router exposing (tagToRoute)
import Types exposing (Tag)
import Utils exposing (null)
import Views.Button exposing (actionButton)


tagSection : List Tag -> List Tag -> Maybe Tag -> Html Msg
tagSection tags globalTags pendingTag =
    div []
        [ h5 [] [ text "Tags" ]
        , if not (isEmpty tags) then
            div
                [ class "tags" ]
                [ ul
                    []
                    (map
                        (\tag ->
                            li
                                [ class "tag" ]
                                [ button
                                    [ onClick <| RemoveTag tag
                                    , class "tagDelete"
                                    ]
                                    [ text "×" ]
                                , a
                                    [ href <| tagToRoute tag ]
                                    [ text tag ]
                                ]
                        )
                        tags
                    )
                ]

          else
            null
        , let
            datalistId =
                "tagDatalist"

            pendTag =
                Maybe.withDefault "" pendingTag
          in
          form
            [ class "tagInput", onSubmit AddTag ]
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
                , value pendTag
                , list datalistId
                , placeholder "Add tag"
                , autocomplete False
                , attribute "autocapitalize" "off"
                , attribute "enterkeyhint" "done"
                , spellcheck False
                ]
                []
            , actionButton
                [ disabled <| pendTag == "" ]
                [ text "+" ]
            ]
        ]
