module Views.EntryList exposing (entryList)

import Dict exposing (Dict, get)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import List exposing (indexedMap)
import Maybe exposing (withDefault)
import Model exposing (BookMap, EntryMap, EntryTab(..), Id, NeighborMap)
import Msg exposing (Msg)
import Utils exposing (pluckIds)
import Views.Entry exposing (entryView)


entryList :
    List Id
    -> EntryMap
    -> BookMap
    -> NeighborMap
    -> Dict Id Bool
    -> Dict Id EntryTab
    -> Html Msg
entryList ids entries books neighbors idToShowDetails idToActiveTab =
    Keyed.ul
        [ class "entries" ]
        (indexedMap
            (\i entry ->
                ( entry.id
                , entryView
                    entries
                    books
                    neighbors
                    (withDefault False (get entry.id idToShowDetails))
                    (withDefault Related (get entry.id idToActiveTab))
                    i
                    entry
                )
            )
            (pluckIds entries ids)
        )
