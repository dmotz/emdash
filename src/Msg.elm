module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Browser.Dom exposing (Element, Error)
import Debounce
import File exposing (File)
import Http
import Time exposing (Posix)
import Types
    exposing
        ( Author
        , Book
        , BookMap
        , BookSort
        , Excerpt
        , ExcerptMap
        , ExcerptSort
        , ExcerptTab
        , Id
        , Lens
        , PendingExcerpt
        , ScorePairs
        , SearchMode
        , StoredModel
        , Tag
        , TagSort
        )
import Url exposing (Url)


type Msg
    = NoOp
    | RestoreState (Maybe StoredModel) Bool
    | MergeNewExcerpts ExcerptMap BookMap
    | ParseJsonText Bool String
    | ParseCsvText String
    | ShowRandom
    | GotRandomIndex Int
    | DragEnter
    | DragLeave
    | GotFile (String -> Msg) File
    | GotDroppedFile File
    | LoadKindleFile String
    | PickKindleFile
    | DeleteExcerpt Excerpt
    | DeleteBook Book
    | EnterBookEditMode
    | ExitBookEditMode
    | SetPendingBookTitle String
    | SetPendingBookAuthor String
    | SetBookEdits
    | UpdateNotes Id String
    | UpdateBookNotes Id String
    | SetBookmark Id Id
    | ToggleFavorite Excerpt
    | UpdatePendingTag Tag
    | AddTag
    | RemoveTag Tag
    | SetRating Book Float
    | SetTagSort TagSort
    | Sort
    | ToggleTagHeader
    | ScrollToElement (Result Error Element)
    | ExportJson
    | ImportJson
    | ImportCsv
    | SyncState StoredModel
    | ClearModal
    | ShowConfirmation String Msg
    | ExportEpub Posix
    | RequestEmbeddings
    | ReceiveEmbeddings (List Id)
    | ReceiveBookEmbeddings
    | ReceiveAuthorEmbeddings
    | ReceiveNeighbors ( Id, ScorePairs )
    | ReceiveBookNeighbors ( Id, ScorePairs )
    | ReceiveAuthorNeighbors ( Author, ScorePairs )
    | ReceiveSemanticSearch ( String, ScorePairs )
    | ReceiveSemanticRank ( Id, ScorePairs )
    | SetSemanticThreshold String
    | LinkClicked UrlRequest
    | UrlChanged Url
    | SortBooks BookSort
    | SortExcerpts ExcerptSort
    | SetExcerptTab Excerpt ExcerptTab Bool
    | ScrollToTop
    | HideHoverUiState Id
    | OnSearchStart String
    | OnSearchEnd String
    | SetSearchTab SearchMode
    | ReceiveUnicodeNormalized String
    | DebounceMsg Debounce.Msg
    | StartDemo
    | GotDemoData (Result Http.Error String)
    | GotLandingData ( List ( String, String ), List Int )
    | GetTime (Posix -> Msg)
    | UpdatePendingExcerpt PendingExcerpt
    | PendingTitleBlur
    | CreateExcerpt PendingExcerpt Posix
    | SubscribeToMailingList
    | UpdateMailingListEmail String
    | ReceiveLensText Id Lens (Result Http.Error String)
