module Views.Snippet exposing (snippetView)

import Dict exposing (get)
import Html exposing (Html, a, blockquote, div, li, mark, text)
import Html.Attributes exposing (class, href)
import List exposing (indexedMap)
import Model exposing (Book, BookMap, Excerpt)
import Msg exposing (Msg)
import Regex
import Router exposing (excerptToRoute)
import String exposing (split)
import Utils exposing (null, rx_)
import Views.Citation exposing (citation)


snippetView : BookMap -> Maybe Float -> Maybe String -> Excerpt -> Html Msg
snippetView books mScore query excerpt =
    case get excerpt.bookId books of
        Just book ->
            let
                inner =
                    innerSnippet excerpt book mScore query
            in
            li [ class "snippet" ]
                [ a
                    [ href <| excerptToRoute books excerpt ]
                    (inner
                        ++ [ div [ class "clone" ] inner ]
                    )
                ]

        _ ->
            null


innerSnippet : Excerpt -> Book -> Maybe Float -> Maybe String -> List (Html Msg)
innerSnippet excerpt book mScore query =
    [ blockquote []
        (case query of
            Just q ->
                addHighlighting excerpt.text q

            _ ->
                [ text excerpt.text ]
        )
    , citation excerpt book mScore
    ]


addHighlighting : String -> String -> List (Html msg)
addHighlighting str query =
    str
        |> Regex.replace
            (rx_ ("\\b" ++ query))
            (\{ match } -> sigil ++ match ++ sigil)
        |> split sigil
        |> indexedMap
            (\i s ->
                if modBy 2 i == 1 then
                    mark [] [ text s ]

                else
                    text s
            )


sigil : String
sigil =
    "__marginalia_splitter__"
