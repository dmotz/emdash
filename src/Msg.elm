module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Browser.Dom exposing (Element, Error)
import Debounce
import File exposing (File)
import Model exposing (Book, BookSort, EntryTab, Id, InputFocus, Tag, TagSort)
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
    | SetLastRead Id Id
    | UpdatePendingTag Tag
    | AddTag
    | RemoveTag Tag
    | SetRating Book Int
    | SetTagSort TagSort
    | Sort
    | ToggleTagHeader
    | ScrollToElement (Result Error Element)
    | ExportJson
    | ImportJson
    | JsonFileLoad String
    | ResetError
    | ExportEpub
    | RequestEmbeddings
    | ReceiveEmbeddings (List Id)
    | ReceiveBookEmbeddings ()
    | ReceiveNeighbors ( Id, List ( Id, Float ) )
    | ReceiveBookNeighbors ( Id, List ( Id, Float ) )
    | ReceiveSemanticSearch ( String, List ( Id, Float ) )
    | SetSemanticThreshold String
    | LinkClicked UrlRequest
    | UrlChanged Url
    | SortBooks BookSort
    | SetEntryTab Id EntryTab Bool
    | ScrollToTop
    | OnScroll Float
    | OnSearchStart String
    | OnSearchEnd String
    | ReceiveUnicodeNormalized String
    | DebounceMsg Debounce.Msg
