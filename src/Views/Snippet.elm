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
        , meter
        , span
        , text
        )
import Html.Attributes exposing (class, href, value)
import Html.Parser
import Html.Parser.Util
import Model exposing (Book, BookMap, Entry, EntryTab(..), InputFocus(..))
import Msg exposing (Msg(..))
import Regex
import Router exposing (entryToRoute)
import String exposing (fromFloat, fromInt)


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
        ([ span [ class "title" ] [ text book.title ]
         , span [ class "divider" ] [ text " - " ]
         , span [] [ text book.author ]
         ]
            ++ (if entry.page /= -1 then
                    [ span [ class "divider" ]
                        [ text " - " ]
                    , span
                        []
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
    ]


addHighlighting : String -> String -> List (Html msg)
addHighlighting str query =
    let
        rx =
            Regex.fromStringWith
                { caseInsensitive = True, multiline = False }
                ("\\b" ++ query)
                |> Maybe.withDefault Regex.never

        addTag m =
            "<mark>" ++ .match m ++ "</mark>"
    in
    case Html.Parser.run <| Regex.replace rx addTag str of
        Ok parsedNodes ->
            Html.Parser.Util.toVirtualDom parsedNodes

        _ ->
            [ text str ]
