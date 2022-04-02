module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Browser.Dom exposing (Error)
import File exposing (File)
import Model exposing (BookSort, Entry, Filter, Id, InputFocus, Tag)
import Url exposing (Url)
import Utils exposing (ClickWithKeys, KeyEvent)


type Msg
    = NoOp
    | SelectEntries (List Entry)
    | ShowRandom
    | ShowByIndex Int
    | ShowNext
    | ShowPrev
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
    | HideEntries (List Entry)
    | PromptHide
    | CancelHide
    | UpdateNotes String
    | UpdatePendingTag Tag
    | AddTag
    | RemoveTag Tag
    | Sort
    | GotDomEl (Result Error (List Float))
    | DidScroll (Result Error ())
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
