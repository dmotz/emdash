module Model exposing
    ( Author
    , Book
    , BookMap
    , BookSort(..)
    , Entry
    , EntryMap
    , EntryTab(..)
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
    | SearchPage String Bool (List Book) (List Entry) (List ( Id, Float ))
    | TitlePage Book (List Entry)
    | AuthorPage Author (List Book)
    | EntryPage Entry Book
    | NotFoundPage String
    | SettingsPage
    | LandingPage
    | ImportPage


type InputFocus
    = NoteFocus
    | TagFocus
    | SearchFocus


type BookSort
    = RecencySort
    | TitleSort
    | NumSort
    | RatingSort


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
    , rating : Float
    , sortIndex : Int
    , tags : List Tag
    , slug : String
    }


type alias Entry =
    { id : Id
    , text : String
    , bookId : Id
    , date : Int
    , page : Int
    , notes : String
    , isFavorite : Bool
    }


type alias EntryMap =
    Dict Id Entry


type alias BookMap =
    Dict Id Book


type alias NeighborMap =
    Dict Id (List ( Id, Float ))


type alias Model =
    { page : Page
    , demoMode : Bool
    , entries : EntryMap
    , books : BookMap
    , semanticThreshold : Float
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
    , pendingTag : Maybe Tag
    , isDragging : Bool
    , reverseSort : Bool
    , inputFocused : Maybe InputFocus
    , parsingError : Bool
    , url : Url
    , key : Nav.Key
    , bookSort : BookSort
    , bookIdToLastRead : Dict Id Id
    , idToShowDetails : Dict Id Bool
    , idToActiveTab : Dict Id EntryTab
    , searchQuery : String
    , searchDebounce : Debounce String
    }


type alias StoredModel =
    { entries : List Entry
    , books : List Book
    , hiddenEntries : List Id
    , bookIdToLastRead : List ( Id, Id )
    }


initialStoredModel : StoredModel
initialStoredModel =
    { entries = []
    , books = []
    , hiddenEntries = []
    , bookIdToLastRead = []
    }
