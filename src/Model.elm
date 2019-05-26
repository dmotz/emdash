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

import InfiniteList
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
    , notes : String
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
    , aboutMode : Bool
    , isDragging : Bool
    , inputFocused : Bool
    , parsingError : Bool
    , uiSize : ( Int, Int )
    , infiniteList : InfiniteList.Model
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
    , aboutMode = False
    , isDragging = False
    , inputFocused = False
    , parsingError = False
    , uiSize = ( 1, 1 )
    , infiniteList = InfiniteList.init
    }


type alias StoredModel =
    { entries : List Entry
    , currentEntry : Maybe Entry
    , hiddenEntries : List Id
    }


initialStoredModel : StoredModel
initialStoredModel =
    { entries = []
    , currentEntry = Nothing
    , hiddenEntries = []
    }
