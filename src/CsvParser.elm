module CsvParser exposing (parse)

import Csv.Decode
    exposing
        ( FieldNames(..)
        , blank
        , column
        , decodeCsv
        , errorToString
        , int
        , into
        , pipeline
        , string
        )
import Dict exposing (insert, update)
import List exposing (foldr)
import Maybe exposing (withDefault)
import Model exposing (BookMap, ExcerptMap)
import Utils exposing (makeExcerpt)


parse : String -> Result String ( ExcerptMap, BookMap )
parse raw =
    case
        decodeCsv
            NoFieldNames
            (into
                (\text title author page date notes ->
                    { title = title
                    , author = author
                    , text = text
                    , page = page
                    , date = date
                    , notes = withDefault "" notes
                    }
                )
                |> pipeline (column 0 string)
                |> pipeline (column 1 string)
                |> pipeline (column 2 string)
                |> pipeline (column 3 (blank int))
                |> pipeline (column 4 (blank int))
                |> pipeline (column 5 (blank string))
            )
            raw
    of
        Ok rows ->
            Ok <|
                foldr
                    (\row ( excerpts, books ) ->
                        let
                            ( excerpt, book ) =
                                makeExcerpt
                                    row.title
                                    row.author
                                    row.text
                                    row.page
                                    row.date
                                    row.notes
                        in
                        ( insert excerpt.id excerpt excerpts
                        , update
                            book.id
                            (\mBook ->
                                Just <|
                                    case mBook of
                                        Just b ->
                                            { b
                                                | sortIndex =
                                                    max
                                                        book.sortIndex
                                                        excerpt.date
                                            }

                                        _ ->
                                            book
                            )
                            books
                        )
                    )
                    ( Dict.empty, Dict.empty )
                    rows

        Err e ->
            Err <| errorToString e
