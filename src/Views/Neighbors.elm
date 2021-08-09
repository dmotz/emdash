module Views.Neighbors exposing (neighbors)

import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , blockquote
        , details
        , div
        , li
        , span
        , summary
        , text
        , ul
        )
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (..)
import List exposing (map, take)
import Model exposing (Entry, Filter(..), Id)
import Msg exposing (Msg(..))
import String exposing (fromFloat, join, words)
import Utils exposing (formatNumber)


neighbors :
    Entry
    -> Dict Id (List ( Entry, Float ))
    -> Bool
    -> Int
    -> Int
    -> Html Msg
neighbors entry neighborMap embeddingsReady completed total =
    details [ id "related" ]
        [ summary [] [ text "Related" ]
        , case Dict.get entry.id neighborMap of
            Just entries ->
                ul
                    []
                    (map
                        (\( neighbor, score ) ->
                            li
                                [ onClick <| SelectEntries [ neighbor ]
                                , class "neighbor"
                                ]
                                [ div
                                    [ class "score" ]
                                    [ div
                                        [ style
                                            "width"
                                            (fromFloat (score * 100) ++ "%")
                                        ]
                                        []
                                    ]
                                , blockquote
                                    []
                                    [ neighbor.text
                                        |> words
                                        |> take 40
                                        |> (\xs -> xs ++ [ "…" ])
                                        |> join " "
                                        |> text
                                    ]
                                , Html.cite
                                    [ id "meta" ]
                                    [ span
                                        [ class "title"
                                        , onClick <|
                                            FilterBy
                                                TitleFilter
                                                neighbor.title
                                        ]
                                        [ text neighbor.title ]
                                    , span [ class "sep" ] [ text "•" ]
                                    , span
                                        [ class "author"
                                        , onClick <|
                                            FilterBy
                                                AuthorFilter
                                                neighbor.author
                                        ]
                                        [ text neighbor.author ]
                                    ]
                                ]
                        )
                        entries
                    )

            _ ->
                div []
                    [ text <|
                        if embeddingsReady then
                            ""

                        else
                            "analyzing excerpts ("
                                ++ formatNumber completed
                                ++ " / "
                                ++ formatNumber total
                                ++ ")"
                    ]
        ]
