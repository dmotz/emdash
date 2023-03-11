module Model exposing
    ( Author
    , Book
    , BookMap
    , BookSort(..)
    , Excerpt
    , ExcerptMap
    , ExcerptSort(..)
    , ExcerptTab(..)
    , Id
    , Model
    , NeighborMap
    , Page(..)
    , PendingExcerpt
    , ScorePairs
    , SearchMode(..)
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
    | SearchPage String SearchMode (List Book) (List Excerpt) ScorePairs
    | TitlePage Book (List Excerpt)
    | AuthorPage Author (List Book)
    | ExcerptPage Excerpt Book
    | NotFoundPage String
    | SettingsPage
    | LandingPage (List Book) Bool
    | ImportPage
    | CreatePage PendingExcerpt (List Title) (List Author)


type BookSort
    = RecencySort
    | TitleSort
    | NumSort
    | RatingSort
    | FavSort


type ExcerptSort
    = ExcerptPageSort
    | ExcerptFavSort
    | ExcerptSemanticSort


type TagSort
    = TagAlphaSort
    | TagNumSort


type ExcerptTab
    = Related
    | Notes
    | Etc


type SearchMode
    = TextMatches
    | SemanticMatches


type alias Id =
    String


type alias Title =
    String


type alias Author =
    String


type alias Tag =
    String


type alias ScorePairs =
    List ( Id, Float )


type alias Book =
    { id : Id
    , title : Title
    , authors : List Author
    , count : Int
    , rating : Float
    , sortIndex : Int
    , tags : List Tag
    , slug : String
    , favCount : Int
    }


type alias Excerpt =
    { id : Id
    , text : String
    , bookId : Id
    , date : Int
    , page : Int
    , notes : String
    , isFavorite : Bool
    }


type alias PendingExcerpt =
    { title : Title
    , author : Author
    , text : String
    , page : Maybe Int
    }


type alias ExcerptMap =
    Dict Id Excerpt


type alias BookMap =
    Dict Id Book


type alias NeighborMap =
    Dict Id ScorePairs


type alias Model =
    { page : Page
    , demoMode : Bool
    , excerpts : ExcerptMap
    , books : BookMap
    , semanticThreshold : Float
    , neighborMap : NeighborMap
    , bookNeighborMap : NeighborMap
    , authorNeighborMap : NeighborMap
    , semanticRankMap : NeighborMap
    , hiddenExcerpts : Set Id
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
    , modalMessage : Maybe ( String, Bool )
    , url : Url
    , key : Nav.Key
    , bookSort : BookSort
    , excerptSort : ExcerptSort
    , bookmarks : Dict Id Id
    , idToShowDetails : Dict Id Bool
    , idToActiveTab : Dict Id ExcerptTab
    , searchQuery : String
    , searchDebounce : Debounce String
    , version : String
    , mailingListUrl : String
    , mailingListField : String
    , mailingListEmail : String
    }


type alias StoredModel =
    { excerpts : List Excerpt
    , books : List Book
    , hiddenExcerpts : List Id
    , bookmarks : List ( Id, Id )
    }


initialStoredModel : StoredModel
initialStoredModel =
    { excerpts = []
    , books = []
    , hiddenExcerpts = []
    , bookmarks = []
    }
