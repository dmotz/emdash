module Model exposing
    ( Book
    , Entry
    , Model
    , StoredModel
    , Tag
    , initialModel
    , initialStoredModel
    )

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
    , tags : List Tag
    }


type alias Model =
    { entries : List Entry
    , shownEntries : Maybe (List Entry)
    , titles : List Book
    , titleFilter : Maybe Book
    , searchFilter : Maybe String
    , parsingError : Bool
    , isDragging : Bool
    , timeOffset : Int
    , tags : List Tag
    , hiddenEntries : Set Id
    , hiddenBooks : Set Book
    , currentEntry : Maybe Entry
    , pendingTag : Maybe Tag
    , isLoading : Bool
    , focusMode : Bool
    , inputFocused : Bool
    , currentIndex : Int
    }


initialModel : Model
initialModel =
    { entries = []
    , shownEntries = Nothing
    , titles = []
    , titleFilter = Nothing
    , searchFilter = Nothing
    , parsingError = False
    , isDragging = False
    , timeOffset = 0
    , tags = []
    , hiddenEntries = Set.empty
    , hiddenBooks = Set.empty
    , currentEntry = Nothing
    , pendingTag = Nothing
    , isLoading = True
    , focusMode = False
    , inputFocused = False
    , currentIndex = 0
    }


type alias StoredModel =
    { entries : List Entry
    , currentEntry : Maybe Entry
    , hiddenEntries : List Id
    , tags : List Tag
    }


initialStoredModel : StoredModel
initialStoredModel =
    { entries = []
    , currentEntry = Nothing
    , hiddenEntries = []
    , tags = []
    }
