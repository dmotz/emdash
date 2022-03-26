module Model exposing
    ( Author
    , Book
    , BookSort(..)
    , Entry
    , Filter(..)
    , Id
    , InputFocus(..)
    , Model
    , StoredModel
    , Tag
    , Title
    , filterToString
    , initialStoredModel
    , sortToString
    , stringToFilter
    , stringToSort
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import InfiniteList as IL
import Set exposing (Set)
import Url exposing (Url)


type Filter
    = TitleFilter
    | AuthorFilter
    | TagFilter
    | TextFilter


type InputFocus
    = NoteFocus
    | TagFocus
    | SearchFocus


type BookSort
    = RecencySort
    | TitleSort
    | NumSort


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
    }


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
    , idsToEntries : Dict Id Entry
    , neighborMap : Dict Id (List ( Entry, Float ))
    , shownEntries : Maybe (List Entry)
    , hiddenEntries : Set Id
    , selectedEntries : List Entry
    , completedEmbeddings : Set Id
    , embeddingsReady : Bool
    , titles : List Title
    , authors : List Author
    , books : List Book
    , bookMap : Dict Id Book
    , tags : List Tag
    , titleTimeSort : Dict Title Int
    , titleRouteMap : Dict String Book
    , filterType : Filter
    , filterValue : Maybe String
    , pendingTag : Maybe Tag
    , focusMode : Bool
    , aboutMode : Bool
    , isDragging : Bool
    , reverseList : Bool
    , hidePromptActive : Bool
    , inputFocused : Maybe InputFocus
    , parsingError : Bool
    , uiSize : ( Int, Int )
    , infiniteList : IL.Model
    , schemaVersion : Int
    , url : Url
    , key : Nav.Key
    , bookSort : BookSort
    , bookSortOrder : Bool
    }


type alias StoredModel =
    { entries : List Entry
    , selectedEntries : List Id
    , hiddenEntries : List Id
    , filterType : String
    , filterValue : Maybe String
    , focusMode : Bool
    , reverseList : Bool
    , schemaVersion : Int
    }


initialStoredModel : StoredModel
initialStoredModel =
    { entries = []
    , selectedEntries = []
    , hiddenEntries = []
    , filterType = "title"
    , filterValue = Nothing
    , focusMode = False
    , reverseList = False
    , schemaVersion = 0
    }


stringToFilter : String -> Filter
stringToFilter s =
    case s of
        "title" ->
            TitleFilter

        "author" ->
            AuthorFilter

        "tag" ->
            TagFilter

        _ ->
            TextFilter


filterToString : Filter -> String
filterToString f =
    case f of
        TitleFilter ->
            "title"

        AuthorFilter ->
            "author"

        TagFilter ->
            "tag"

        _ ->
            "text"


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
            "number of entries"
