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
        , Excerpt
        , ExcerptSort
        , ExcerptTab
        , Id
        , InputFocus
        , PendingExcerpt
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
    | GotFile (String -> Msg) File
    | LoadKindleFile String
    | PickKindleFile
    | KeyDown KeyEvent
    | SetInputFocus (Maybe InputFocus)
    | HideExcerpt Excerpt
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
    | SortExcerpts ExcerptSort
    | SetExcerptTab Id ExcerptTab Bool
    | ScrollToTop
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
