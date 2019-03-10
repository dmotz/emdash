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
    | TagFilter
    | TextFilter


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
    , hiddenEntries : Set Id
    , currentEntry : Maybe Entry
    , titles : List Title
    , authors : List Author
    , tags : List Tag
    , filterType : Filter
    , filterValue : Maybe String
    , pendingTag : Maybe Tag
    , focusMode : Bool
    , editMode : Bool
    , isDragging : Bool
    , inputFocused : Bool
    , parsingError : Bool
    }


initialModel : Model
initialModel =
    { entries = []
    , shownEntries = Nothing
    , hiddenEntries = Set.empty
    , currentEntry = Nothing
    , titles = []
    , authors = []
    , tags = []
    , filterType = TitleFilter
    , filterValue = Nothing
    , pendingTag = Nothing
    , focusMode = False
    , editMode = False
    , isDragging = False
    , inputFocused = False
    , parsingError = False
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
