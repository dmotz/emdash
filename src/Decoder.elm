module Decoder exposing (..)

import Json.Decode
    exposing
        ( Error
        , decodeString
        , field
        , index
        , int
        , list
        , map2
        , map5
        , map6
        , map8
        , string
        )
import Model exposing (Book, Entry, StoredModel)
import Tuple exposing (pair)


decodeStoredModel : String -> Result Error StoredModel
decodeStoredModel =
    map5
        StoredModel
        (field
            "entries"
            (list
                (map6
                    Entry
                    (field "id" string)
                    (field "text" string)
                    (field "bookId" string)
                    (field "date" int)
                    (field "page" int)
                    (field "notes" string)
                )
            )
        )
        (field
            "books"
            (list
                (map8
                    Book
                    (field "id" string)
                    (field "title" string)
                    (field "author" string)
                    (field "count" int)
                    (field "rating" int)
                    (field "sortIndex" int)
                    (field "tags" (list string))
                    (field "slug" string)
                )
            )
        )
        (field "hiddenEntries" (list string))
        (field "schemaVersion" int)
        (field
            "bookIdToLastRead"
            (list (map2 pair (index 0 string) (index 1 string)))
        )
        |> decodeString
