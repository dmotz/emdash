module Views.EmbeddingProgress exposing (embeddingProgress)

import Html exposing (Html, div, p, progress, text)
import Html.Attributes exposing (class, value)
import Msg exposing (Msg)
import String exposing (fromFloat)
import Utils exposing (formatNumber)


embeddingProgress : Int -> Int -> Html Msg
embeddingProgress done total =
    div
        [ class "embeddingProgress" ]
        [ p [] [ text "Analyzing excerpts…" ]
        , div
            []
            [ progress
                [ Html.Attributes.max "1"
                , value <| fromFloat (toFloat done / toFloat total)
                ]
                []
            ]
        , text <| formatNumber done ++ " / " ++ formatNumber total
        ]
