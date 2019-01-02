module Model exposing (Entry, Model, initialModel)

import Set exposing (..)


type alias Tag =
    String


type alias Id =
    String


type alias Book =
    String


type alias Entry =
    { id : Id
    , text : String
    , title : Book
    , author : String
    , page : Maybe Int
    }


type alias Model =
    { entries : List Entry
    , shownEntries : List Entry
    , titles : List Book
    , timeOffset : Int
    , tags : Set Tag
    , hiddenEntries : Set Id
    , currentEntry : Maybe Entry
    , isLoading : Bool
    , focusMode : Bool
    }


initialModel : Model
initialModel =
    { entries = []
    , shownEntries = []
    , titles = []
    , timeOffset = 0
    , tags = Set.empty
    , hiddenEntries = Set.empty
    , currentEntry = Nothing
    , isLoading = True
    , focusMode = False
    }
