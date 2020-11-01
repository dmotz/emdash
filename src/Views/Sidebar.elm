module Views.Sidebar exposing (sidebar, sidebarId)

import Html exposing (Html, blockquote, div, li, nav, text, ul)
import Html.Attributes exposing (class, classList, id, style)
import Html.Parser
import Html.Parser.Util
import InfiniteList as IL
import Json.Decode as Decode
import List exposing (head, length, map)
import Maybe exposing (withDefault)
import Model exposing (Entry)
import Msg exposing (Msg(..))
import Regex
import Set
import String exposing (slice)
import Utils exposing (ClickWithKeys, charLimit, getEntryHeight, queryCharMin)
import Views.Common exposing (on)


sidebar :
    IL.Model
    -> ( Int, Int )
    -> List Entry
    -> Maybe String
    -> Bool
    -> List Entry
    -> Html Msg
sidebar infiniteList uiSize entries query showTitles selectedEntries =
    nav
        [ id sidebarId
        , classList [ ( "no-titles", not showTitles ) ]
        , IL.onScroll InfList
        ]
        [ if length entries == 0 then
            div [ class "no-results" ] [ text "no results" ]

          else
            IL.view
                (listViewConfig uiSize selectedEntries query showTitles)
                infiniteList
                entries
        ]


entriesContainer : List ( String, String ) -> List (Html msg) -> Html msg
entriesContainer styles children =
    ul (map (\( k, v ) -> style k v) styles) children


addHighlighting : String -> String -> List (Html msg)
addHighlighting str query =
    let
        rx =
            Regex.fromStringWith
                { caseInsensitive = True, multiline = False }
                ("\\b" ++ query)
                |> Maybe.withDefault Regex.never

        index =
            Regex.find rx str |> map .index |> head |> withDefault 0

        addTag m =
            "<span class=\"highlight\">" ++ .match m ++ "</span>"

        excerpt =
            let
                trunc =
                    if (index + String.length query) > charLimit then
                        "…" ++ slice index (String.length str) str

                    else
                        str
            in
            trunc
    in
    case Html.Parser.run <| Regex.replace rx addTag excerpt of
        Ok parsedNodes ->
            Html.Parser.Util.toVirtualDom parsedNodes

        _ ->
            [ text str ]


listEntry :
    Maybe String
    -> Bool
    -> List Entry
    -> Int
    -> Int
    -> Entry
    -> Html Msg
listEntry query showTitles selectedEntries _ _ entry =
    let
        selectedIds =
            selectedEntries |> map .id |> Set.fromList
    in
    li
        [ id entry.id
        , Decode.map3 ClickWithKeys
            (Decode.field "ctrlKey" Decode.bool)
            (Decode.field "metaKey" Decode.bool)
            (Decode.field "shiftKey" Decode.bool)
            |> Decode.map (EntryClick entry)
            |> on "click"
        ]
        [ if Set.member entry.id selectedIds then
            div [ class "active-entry" ] []

          else
            text ""
        , blockquote
            []
            (case query of
                Nothing ->
                    [ text entry.text ]

                Just q ->
                    if String.length q < queryCharMin then
                        [ text entry.text ]

                    else
                        addHighlighting entry.text q
            )
        , if showTitles then
            Html.cite [ class "title" ] [ text entry.title ]

          else
            text ""
        ]


listViewConfig :
    ( Int, Int )
    -> List Entry
    -> Maybe String
    -> Bool
    -> IL.Config Entry Msg
listViewConfig ( _, h ) selectedEntries query showTitles =
    IL.config
        { itemView = listEntry query showTitles selectedEntries
        , itemHeight =
            IL.withConstantHeight <| getEntryHeight showTitles
        , containerHeight = h
        }
        |> IL.withCustomContainer entriesContainer


sidebarId : String
sidebarId =
    "sidebar"
