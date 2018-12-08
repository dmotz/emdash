module Model exposing (Entry, Model, initialModel)

import Set exposing (..)


type alias Tag =
    String


type alias Id =
    String


type alias Entry =
    { text : String
    , title : String
    , author : String
    , meta : String
    }


type alias Model =
    { entries : List Entry
    , timeOffset : Int
    , tags : Set Tag
    , hiddenEntries : Set Id
    , currentEntry : Maybe Entry
    , isLoading : Bool
    }


initialModel : Model
initialModel =
    { entries = []
    , timeOffset = 0
    , tags = Set.empty
    , hiddenEntries = Set.empty
    , currentEntry = Nothing
    , isLoading = True
    }
