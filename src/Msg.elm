module Msg exposing (Msg(..))

import Http
import Model exposing (Entry)


type Msg
    = OnFetch (Result Http.Error String)
    | ShowEntry Entry
    | ShowRandom
    | ShowByIndex Int
    | OnFilter String
    | FilterTitle String
    | SetFocusMode Bool
