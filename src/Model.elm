module Model exposing
    ( Author
    , Entry
    , Filter(..)
    , Model
    , StoredModel
    , Tag
    , Title
    , initialModel
    , initialStoredModel
    )

import Set exposing (Set)


type Filter
    = TitleFilter
    | AuthorFilter
    | TextFilter
    | TagFilter


type alias Id =
    String


type alias Title =
    String


type alias Author =
    String


type alias Tag =
    String


type alias Entry =
    { id : Id
    , text : String
    , title : Title
    , author : String
    , page : Maybe Int
    , tags : List Tag
    }


type alias Model =
    { entries : List Entry
    , shownEntries : Maybe (List Entry)
    , titles : List Title
    , authors : List Author
    , filterType : Filter
    , filterValue : Maybe String
    , parsingError : Bool
    , isDragging : Bool
    , tags : List Tag
    , hiddenEntries : Set Id
    , currentEntry : Maybe Entry
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
    , filterType = AuthorFilter
    , filterValue = Nothing
    , parsingError = False
    , isDragging = False
    , tags = []
    , hiddenEntries = Set.empty
    , currentEntry = Nothing
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
