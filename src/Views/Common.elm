module Views.Common exposing (on, repoUrl)

import Html exposing (Attribute)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)


repoUrl : String
repoUrl =
    "https://github.com/dmotz/marginalia"


on : String -> Decoder msg -> Attribute msg
on event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
