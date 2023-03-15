module Views.Citation exposing (citation)

import Html exposing (Html, a, cite, div, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (stopPropagationOn)
import Json.Decode exposing (succeed)
import List exposing (intersperse, map)
import Msg exposing (Msg(..))
import Router exposing (authorToRoute, titleSlugToRoute)
import String exposing (fromInt)
import Types exposing (Book, Excerpt)
import Utils exposing (formatScore, getExcerptDomId)


citation : Excerpt -> Book -> Maybe Float -> Html Msg
citation excerpt book mScore =
    cite
        []
        ([ div []
            [ a
                [ class "title", href <| titleSlugToRoute book.slug, stopLinkProp ]
                [ text book.title ]
            , span [ class "divider" ] [ text "•" ]
            ]
         , div
            []
            ((map
                (\author ->
                    a
                        [ href <| authorToRoute author, stopLinkProp ]
                        [ text author ]
                )
                book.authors
                |> intersperse (text " / ")
             )
                ++ (if excerpt.page /= -1 then
                        [ span [ class "divider" ] [ text "•" ] ]

                    else
                        []
                   )
            )
         ]
            ++ (if excerpt.page /= -1 then
                    [ a
                        [ href <|
                            titleSlugToRoute book.slug
                                ++ "#"
                                ++ getExcerptDomId excerpt.id
                        , stopLinkProp
                        ]
                        [ text <| "p. " ++ fromInt excerpt.page ]
                    ]

                else
                    []
               )
            ++ (case mScore of
                    Just score ->
                        [ div
                            [ class "score" ]
                            [ span [] [ formatScore score ]
                            , div [ class "hint" ] [ text "Similarity score" ]
                            ]
                        ]

                    _ ->
                        []
               )
        )


stopLinkProp : Html.Attribute Msg
stopLinkProp =
    stopPropagationOn "click" (succeed ( NoOp, True ))
