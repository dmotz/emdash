module Views.ExcerptList exposing (excerptList)

import Dict exposing (Dict, get)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import List exposing (indexedMap)
import Maybe exposing (withDefault)
import Model exposing (BookMap, Excerpt, ExcerptMap, ExcerptTab(..), Id, NeighborMap)
import Msg exposing (Msg)
import Views.Excerpt exposing (excerptView)


excerptList :
    List Excerpt
    -> ExcerptMap
    -> BookMap
    -> NeighborMap
    -> Dict Id Bool
    -> Dict Id ExcerptTab
    -> Id
    -> Maybe (Html Msg)
    -> Html Msg
excerptList excerpts excerptMap books neighbors idToShowDetails idToActiveTab bookmark mProgress =
    Keyed.ul
        [ class "excerpts" ]
        (indexedMap
            (\i excerpt ->
                ( excerpt.id
                , excerptView
                    excerptMap
                    books
                    (withDefault [] (get excerpt.id neighbors))
                    (withDefault False (get excerpt.id idToShowDetails))
                    (withDefault Related (get excerpt.id idToActiveTab))
                    i
                    False
                    (bookmark == excerpt.id)
                    mProgress
                    excerpt
                )
            )
            excerpts
        )