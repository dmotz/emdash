module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Browser.Dom exposing (Element, Error)
import Debounce
import File exposing (File)
import Model exposing (BookSort, EntryTab, Filter, Id, InputFocus, Tag)
import Url exposing (Url)
import Utils exposing (KeyEvent)


type Msg
    = NoOp
    | ShowRandom
    | FilterBy (Maybe Filter)
    | ToggleFocusMode
    | ToggleAboutMode
    | DragEnter
    | DragLeave
    | GotFiles (String -> Msg) File (List File)
    | FileLoad String
    | PickFile
    | KeyDown KeyEvent
    | SetInputFocus (Maybe InputFocus)
    | HideEntry Id
    | PromptHide
    | CancelHide
    | UpdateNotes Id String
    | UpdatePendingTag Tag
    | AddTag
    | RemoveTag Tag
    | Sort
    | ScrollToElement (Result Error Element)
    | ExportJson
    | ImportJson
    | JsonFileLoad String
    | ResetError
    | ExportEpub
    | RequestEmbeddings
    | ReceiveEmbeddings (List Id)
    | ReceiveNeighbors ( Id, List ( Id, Float ) )
    | LinkClicked UrlRequest
    | UrlChanged Url
    | SortBooks BookSort
    | OnIntersect Id
    | ToggleDetails Id
    | SetEntryTab Id EntryTab
    | ScrollToTop
    | OnScroll Float
    | OnSearch String
    | ReceiveUnicodeNormalized String
    | DebounceMsg Debounce.Msg
