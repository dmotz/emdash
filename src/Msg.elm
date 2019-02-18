module Msg exposing (Msg(..))

import File exposing (File)
import Model exposing (Entry, Tag)


type Msg
    = ShowEntry Entry
    | ShowRandom
    | ShowByIndex Int
    | ShowNext
    | ShowPrev
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
    | SetInputFocus Bool
    | HideEntry Entry
    | UpdatePendingTag Tag
    | AddTag Tag
