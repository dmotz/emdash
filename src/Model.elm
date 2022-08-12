module Model exposing
    ( Author
    , Book
    , BookMap
    , BookSort(..)
    , Entry
    , EntryMap
    , EntryTab(..)
    , Filter(..)
    , Id
    , InputFocus(..)
    , Model
    , NeighborMap
    , StoredModel
    , Tag
    , TagSort(..)
    , Title
    , initialStoredModel
    , sortToString
    , stringToSort
    )

import Browser.Navigation as Nav
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Set exposing (Set)
import Url exposing (Url)


type Filter
    = TitleFilter Book
    | AuthorFilter Author
    | TagFilter Tag
    | TextFilter String


type InputFocus
    = NoteFocus
    | TagFocus
    | SearchFocus


type BookSort
    = RecencySort
    | TitleSort
    | NumSort


type TagSort
    = TagAlphaSort
    | TagNumSort


type EntryTab
    = Related
    | Notes
    | Etc


type alias Id =
    String


type alias Title =
    String


type alias Author =
    String


type alias Tag =
    String


type alias Book =
    { id : Id
    , title : Title
    , author : Author
    , count : Int
    , sortIndex : Int
    , tags : List Tag
    }


type alias Entry =
    { id : Id
    , text : String
    , bookId : Id
    , date : Int
    , page : Int
    , notes : String
    }


type alias EntryMap =
    Dict Id Entry


type alias BookMap =
    Dict Id Book


type alias NeighborMap =
    Dict Id (List ( Id, Float ))


type alias Model =
    { entries : EntryMap
    , books : BookMap
    , booksShown : List Id
    , entriesShown : Maybe (List Id)
    , neighborMap : NeighborMap
    , bookNeighborMap : NeighborMap
    , hiddenEntries : Set Id
    , completedEmbeddings : Set Id
    , embeddingsReady : Bool
    , titleRouteMap : Dict String Id
    , authorRouteMap : Dict String Author
    , routeNotFound : Bool
    , tags : List Tag
    , tagCounts : Dict Tag Int
    , tagSort : TagSort
    , filter : Maybe Filter
    , pendingTag : Maybe Tag
    , focusMode : Bool
    , aboutMode : Bool
    , isDragging : Bool
    , reverseSort : Bool
    , hidePromptActive : Bool
    , inputFocused : Maybe InputFocus
    , parsingError : Bool
    , schemaVersion : Int
    , url : Url
    , key : Nav.Key
    , bookSort : BookSort
    , bookSortOrder : Bool
    , currentBook : Maybe Id
    , lastTitleSlug : String
    , bookIdToLastRead : Dict Id Id
    , idToShowDetails : Dict Id Bool
    , idToActiveTab : Dict Id EntryTab
    , searchQuery : String
    , hideHeader : Bool
    , searchDebounce : Debounce String
    }


type alias StoredModel =
    { entries : List Entry
    , books : List Book
    , hiddenEntries : List Id
    , focusMode : Bool
    , reverseSort : Bool
    , schemaVersion : Int
    , bookIdToLastRead : List ( Id, Id )
    }


initialStoredModel : StoredModel
initialStoredModel =
    { entries = []
    , books = []
    , hiddenEntries = []
    , focusMode = False
    , reverseSort = False
    , schemaVersion = 0
    , bookIdToLastRead = []
    }


stringToSort : String -> BookSort
stringToSort s =
    case s of
        "recent" ->
            RecencySort

        "title" ->
            TitleSort

        _ ->
            NumSort


sortToString : BookSort -> String
sortToString t =
    case t of
        RecencySort ->
            "recent"

        TitleSort ->
            "title"

        _ ->
            "№ of entries"
