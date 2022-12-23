module Decoder exposing (decodeStoredModel)

import Json.Decode
    exposing
        ( Decoder
        , Error
        , bool
        , decodeString
        , float
        , index
        , int
        , list
        , map2
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (optional, required)
import Model exposing (Book, Entry, StoredModel)
import Tuple exposing (pair)


decodeStoredModel : String -> Result Error StoredModel
decodeStoredModel =
    succeed StoredModel
        |> required "entries" (list entryDecoder)
        |> required "books" (list bookDecoder)
        |> optional "hiddenEntries" (list string) []
        |> optional
            "bookmarks"
            (list (map2 pair (index 0 string) (index 1 string)))
            []
        |> decodeString


entryDecoder : Decoder Entry
entryDecoder =
    succeed Entry
        |> required "id" string
        |> required "text" string
        |> required "bookId" string
        |> optional "date" int 0
        |> optional "page" int -1
        |> optional "notes" string ""
        |> optional "isFavorite" bool False


bookDecoder : Decoder Book
bookDecoder =
    succeed Book
        |> required "id" string
        |> required "title" string
        |> required "author" string
        |> required "count" int
        |> optional "rating" float 0
        |> optional "sortIndex" int 0
        |> optional "tags" (list string) []
        |> required "slug" string
