module Msg exposing (Msg(..))

import Browser.Dom exposing (Error)
import File exposing (File)
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
    | EnterEditMode
    | ExitEditMode
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | FileLoad String
    | PickFile
    | KeyDown KeyEvent
    | SetInputFocus Bool
    | HideEntry Entry
    | UpdatePendingTag Tag
    | AddTag Tag
    | RemoveTag Tag
    | GotDomEl (Result Error (List Float))
    | DidScroll (Result Error ())
