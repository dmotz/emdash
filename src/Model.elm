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
    , Page(..)
    , StoredModel
    , Tag
    , TagSort(..)
    , Title
    , initialStoredModel
    )

import Browser.Navigation as Nav
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Set exposing (Set)
import Url exposing (Url)


type Page
    = MainPage (List Book) (Maybe Tag)
    | SearchPage String (List Book) (List Entry)
    | TitlePage Book (List Entry)
    | AuthorPage Author (List Book)
    | EntryPage Entry Book
    | NotFoundPage String
    | LandingPage


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
    { page : Page
    , entries : EntryMap
    , books : BookMap
    , entriesShown : Maybe (List Id)
    , neighborMap : NeighborMap
    , bookNeighborMap : NeighborMap
    , hiddenEntries : Set Id
    , completedEmbeddings : Set Id
    , embeddingsReady : Bool
    , titleRouteMap : Dict String Id
    , authorRouteMap : Dict String Author
    , tags : List Tag
    , tagCounts : Dict Tag Int
    , tagSort : TagSort
    , showTagHeader : Bool
    , filter : Maybe Filter
    , pendingTag : Maybe Tag
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
    , reverseSort : Bool
    , schemaVersion : Int
    , bookIdToLastRead : List ( Id, Id )
    }


initialStoredModel : StoredModel
initialStoredModel =
    { entries = []
    , books = []
    , hiddenEntries = []

    -- , reverseSort = False
    , schemaVersion = 0
    , bookIdToLastRead = []
    }
