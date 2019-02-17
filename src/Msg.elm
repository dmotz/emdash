module Msg exposing (Msg(..))

import File exposing (File)
import Model exposing (Entry)


type Msg
    = ShowEntry Entry
    | ShowRandom
    | ShowByIndex Int
    | FilterBySearch String
    | FilterByTitle String
    | ToggleFocusMode
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | FileLoad String
    | PickFile
    | KeyDown String
    | SetInputFocus Bool
    | HideEntry Entry
