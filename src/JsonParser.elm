module JsonParser exposing (decodeStoredModel)

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
        , nullable
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (optional, required)
import Model exposing (Book, Excerpt, StoredModel)
import Tuple exposing (pair)


decodeStoredModel : String -> Result Error StoredModel
decodeStoredModel =
    succeed StoredModel
        |> required "excerpts" (list excerptDecoder)
        |> required "books" (list bookDecoder)
        |> optional "hiddenExcerpts" (list string) []
        |> optional
            "bookmarks"
            (list (map2 pair (index 0 string) (index 1 string)))
            []
        |> decodeString


excerptDecoder : Decoder Excerpt
excerptDecoder =
    succeed Excerpt
        |> required "id" string
        |> required "text" string
        |> required "bookId" string
        |> optional "date" int 0
        |> optional "page" int -1
        |> optional "notes" string ""
        |> optional "isFavorite" bool False
        |> optional "sourceUrl" (nullable string) Nothing


bookDecoder : Decoder Book
bookDecoder =
    succeed Book
        |> required "id" string
        |> required "title" string
        |> required "authors" (list string)
        |> required "count" int
        |> optional "rating" float 0
        |> optional "sortIndex" int 0
        |> optional "tags" (list string) []
        |> required "slug" string
        |> optional "favCount" int 0
