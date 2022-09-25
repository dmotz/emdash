module Views.EntryList exposing (entryList)

import Dict exposing (Dict, get)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import List exposing (indexedMap)
import Maybe exposing (withDefault)
import Model exposing (BookMap, Entry, EntryMap, EntryTab(..), Id, NeighborMap)
import Msg exposing (Msg)
import Views.Entry exposing (entryView)


entryList :
    List Entry
    -> EntryMap
    -> BookMap
    -> NeighborMap
    -> Dict Id Bool
    -> Dict Id EntryTab
    -> Html Msg
entryList entries entryMap books neighbors idToShowDetails idToActiveTab =
    Keyed.ul
        [ class "entries" ]
        (indexedMap
            (\i entry ->
                ( entry.id
                , entryView
                    entryMap
                    books
                    neighbors
                    (withDefault False (get entry.id idToShowDetails))
                    (withDefault Related (get entry.id idToActiveTab))
                    i
                    False
                    entry
                )
            )
            entries
        )
