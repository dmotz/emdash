module Views.Snippet exposing (snippetView)

import Dict exposing (get)
import Html
    exposing
        ( Html
        , a
        , blockquote
        , cite
        , div
        , li
        , mark
        , meter
        , span
        , text
        )
import Html.Attributes exposing (class, href, value)
import Html.Events exposing (stopPropagationOn)
import Json.Decode exposing (succeed)
import List exposing (indexedMap)
import Model exposing (Book, BookMap, Entry, EntryTab(..), InputFocus(..))
import Msg exposing (Msg(..))
import Regex
import Router exposing (authorToRoute, entryToRoute, titleToRoute)
import String exposing (fromFloat, fromInt, split)
import Utils exposing (rx_)


snippetView : BookMap -> Maybe Float -> Maybe String -> Entry -> Html Msg
snippetView books mScore query entry =
    case get entry.bookId books of
        Just book ->
            let
                inner =
                    innerSnippet entry book mScore query
            in
            li [ class "snippet" ]
                [ a
                    [ href <| entryToRoute books entry ]
                    (inner
                        ++ [ div [ class "clone" ] inner ]
                    )
                ]

        _ ->
            text ""


innerSnippet : Entry -> Book -> Maybe Float -> Maybe String -> List (Html Msg)
innerSnippet entry book mScore query =
    [ blockquote []
        (case query of
            Just q ->
                addHighlighting entry.text q

            Nothing ->
                [ text entry.text ]
        )
    , cite []
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
                    , span [] [ text <| "p. " ++ fromInt entry.page ]
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


stopLinkProp : Html.Attribute Msg
stopLinkProp =
    stopPropagationOn "click" (succeed ( NoOp, True ))
