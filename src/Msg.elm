module Msg exposing (Msg(..))

import Browser.Dom exposing (Error)
import File exposing (File)
import Model exposing (Entry, Filter, Tag)


type Msg
    = ShowEntry Entry
    | ShowRandom
    | ShowByIndex Int
    | ShowNext
    | ShowPrev
    | SetFilterMode Filter
    | FilterBySearch String
    | FilterByTitle String
    | FilterByTag Tag
    | ToggleFocusMode
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | FileLoad String
    | PickFile
    | KeyDown String
    | KeyUp String
    | SetInputFocus Bool
    | HideEntry Entry
    | UpdatePendingTag Tag
    | AddTag Tag
    | RemoveTag Tag
    | GotDomEl (Result Error (List Float))
    | DidScroll (Result Error ())
