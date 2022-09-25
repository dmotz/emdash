module Views.Citation exposing (citation)

import Html exposing (Html, a, cite, div, meter, span, text)
import Html.Attributes exposing (class, href, value)
import Html.Events exposing (stopPropagationOn)
import Json.Decode exposing (succeed)
import Model exposing (Book, Entry)
import Msg exposing (Msg(..))
import Router exposing (authorToRoute, titleToRoute)
import String exposing (fromFloat, fromInt)
import Utils exposing (getEntryDomId)


citation : Entry -> Book -> Maybe Float -> Html Msg
citation entry book mScore =
    cite []
        ([ a
            [ class "title", href <| titleToRoute book.title, stopLinkProp ]
            [ text book.title ]
         , span [ class "divider" ] [ text " • " ]
         , a
            [ href <| authorToRoute book.author, stopLinkProp ]
            [ text book.author ]
         ]
            ++ (if entry.page /= -1 then
                    [ span [ class "divider" ] [ text " • " ]
                    , a
                        [ href <|
                            titleToRoute book.title
                                ++ "#"
                                ++ getEntryDomId entry.id
                        , stopLinkProp
                        ]
                        [ text <| "p. " ++ fromInt entry.page ]
                    ]

                else
                    []
               )
            ++ (case mScore of
                    Just score ->
                        [ div
                            [ class "score" ]
                            [ span [] [ text (score * 100 |> round |> fromInt) ]
                            , meter [ score |> fromFloat |> value ] []
                            ]
                        ]

                    _ ->
                        []
               )
        )


stopLinkProp : Html.Attribute Msg
stopLinkProp =
    stopPropagationOn "click" (succeed ( NoOp, True ))
