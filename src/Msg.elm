module Msg exposing (Msg(..))

import Browser.Dom exposing (Error)
import File exposing (File)
import InfiniteList as IL
import Model exposing (Entry, Filter, Tag)
import Utils exposing (KeyEvent)


type Msg
    = ShowEntry Entry
    | ShowRandom
    | ShowByIndex Int
    | ShowNext
    | ShowPrev
    | FilterBy Filter String
    | ToggleFocusMode
    | ToggleAboutMode
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | FileLoad String
    | PickFile
    | KeyDown KeyEvent
    | SetInputFocus Bool
    | HideEntry Entry
    | UpdateNotes String
    | UpdatePendingTag Tag
    | AddTag Tag
    | RemoveTag Tag
    | Sort
    | GotDomEl (Result Error (List Float))
    | DidScroll (Result Error ())
    | ExportJson
    | ResetError
    | Resize ( Int, Int )
    | InfList IL.Model
