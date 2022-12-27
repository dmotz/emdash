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
        , EntrySort
        , EntryTab
        , Id
        , InputFocus
        , ScorePairs
        , StoredModel
        , Tag
        , TagSort
        )
import Url exposing (Url)
import Utils exposing (KeyEvent)


type Msg
    = NoOp
    | ShowRandom
    | GotRandomIndex Int
    | DragEnter
    | DragLeave
    | GotFiles (String -> Msg) File (List File)
    | FileLoad String
    | PickFile
    | KeyDown KeyEvent
    | SetInputFocus (Maybe InputFocus)
    | HideEntry Id
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
    | ExportEpub
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
    | ReceiveUnicodeNormalized String
    | DebounceMsg Debounce.Msg
