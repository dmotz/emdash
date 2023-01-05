module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Browser.Dom exposing (Element, Error)
import Debounce
import File exposing (File)
import Http
import Model
    exposing
        ( Book
        , BookSort
        , Entry
        , EntrySort
        , EntryTab
        , Id
        , InputFocus
        , PendingEntry
        , ScorePairs
        , SearchMode
        , StoredModel
        , Tag
        , TagSort
        )
import Time exposing (Posix)
import Url exposing (Url)
import Utils exposing (KeyEvent)


type Msg
    = NoOp
    | RestoreState (Maybe StoredModel) Bool
    | ParseJsonText String
    | ShowRandom
    | GotRandomIndex Int
    | DragEnter
    | DragLeave
    | GotFiles (String -> Msg) File (List File)
    | LoadKindleFile String
    | PickKindleFile
    | KeyDown KeyEvent
    | SetInputFocus (Maybe InputFocus)
    | HideEntry Entry
    | UpdateNotes Id String
    | SetBookmark Id Id
    | ToggleFavorite Id
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
    | ResetError
    | ExportEpub Posix
    | RequestEmbeddings
    | ReceiveEmbeddings (List Id)
    | ReceiveBookEmbeddings ()
    | ReceiveNeighbors ( Id, ScorePairs )
    | ReceiveBookNeighbors ( Id, ScorePairs )
    | ReceiveSemanticSearch ( String, ScorePairs )
    | ReceiveSemanticRank ( Id, ScorePairs )
    | SetSemanticThreshold String
    | LinkClicked UrlRequest
    | UrlChanged Url
    | SortBooks BookSort
    | SortEntries EntrySort
    | SetEntryTab Id EntryTab Bool
    | ScrollToTop
    | OnSearchStart String
    | OnSearchEnd String
    | SetSearchTab SearchMode
    | ReceiveUnicodeNormalized String
    | DebounceMsg Debounce.Msg
    | StartDemo
    | GotDemoData (Result Http.Error String)
    | GetTime (Posix -> Msg)
    | UpdatePendingEntry PendingEntry
    | PendingTitleBlur
    | CreateEntry PendingEntry Posix
