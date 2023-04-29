module Model exposing (ModalMsg(..), Model)

import Browser.Navigation as Nav
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Msg exposing (Msg)
import Set exposing (Set)
import Types
    exposing
        ( Author
        , BookMap
        , BookSort
        , CountMap
        , ExcerptMap
        , ExcerptSort
        , ExcerptTab
        , Id
        , NeighborMap
        , Page
        , Tag
        , TagSort
        )
import Url exposing (Url)


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
    , authorEmbeddingsReady : Bool
    , titleRouteMap : Dict String Id
    , authorRouteMap : Dict String Author
    , excerptCountMap : CountMap
    , favCountMap : CountMap
    , tags : List Tag
    , tagCounts : Dict Tag Int
    , tagSort : TagSort
    , showTagHeader : Bool
    , pendingTag : Maybe Tag
    , isDragging : Bool
    , reverseSort : Bool
    , modalMessage : Maybe ModalMsg
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
    , didJoinMailingList : Bool
    , supportIssues : List String
    , showHoverUi : Bool
    }


type ModalMsg
    = InfoMsg String
    | ErrMsg String
    | InitErrMsg String
    | ConfirmationMsg String Msg
