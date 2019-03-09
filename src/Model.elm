module Model exposing
    ( Author
    , Book
    , Entry
    , Filter(..)
    , Model
    , StoredModel
    , Tag
    , initialModel
    , initialStoredModel
    )

import Set exposing (Set)


type Filter
    = TitleFilter
    | AuthorFilter
    | TextFilter
    | TagFilter


type alias Tag =
    String


type alias Id =
    String


type alias Book =
    String


type alias Author =
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
    , authors : List Author
    , titleFilter : Maybe Book
    , searchFilter : Maybe String
    , parsingError : Bool
    , isDragging : Bool
    , tags : List Tag
    , hiddenEntries : Set Id
    , currentEntry : Maybe Entry
    , filterMode : Filter
    , pendingTag : Maybe Tag
    , focusMode : Bool
    , inputFocused : Bool
    , metaKeyCount : Int
    }


initialModel : Model
initialModel =
    { entries = []
    , shownEntries = Nothing
    , titles = []
    , authors = []
    , titleFilter = Nothing
    , searchFilter = Nothing
    , parsingError = False
    , isDragging = False
    , tags = []
    , hiddenEntries = Set.empty
    , currentEntry = Nothing
    , filterMode = TitleFilter
    , pendingTag = Nothing
    , focusMode = False
    , inputFocused = False
    , metaKeyCount = 0
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
