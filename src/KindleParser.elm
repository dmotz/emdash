module KindleParser exposing (process)

import DateTime exposing (fromRawParts, toPosix)
import Dict exposing (insert, update)
import List exposing (foldr, head, map)
import Maybe exposing (andThen, withDefault)
import Model exposing (BookMap, ExcerptMap)
import Regex exposing (Regex)
import String exposing (lines, repeat, right, split, startsWith, toInt, trim)
import Time exposing (Month(..), posixToMillis)
import Utils exposing (makeExcerpt, rx, rx_)


process : String -> ( ExcerptMap, BookMap )
process =
    lines
        >> foldr folder ( [], [] )
        >> (\( xs, x ) -> x :: xs)
        >> foldr findNotes []
        >> makeDicts


separator : String
separator =
    repeat 10 "="


folder :
    String
    -> ( List (List String), List String )
    -> ( List (List String), List String )
folder line ( blocks, currentBlock ) =
    if line == "" then
        ( blocks, currentBlock )

    else if line == separator then
        ( currentBlock :: blocks, [] )

    else
        ( blocks, currentBlock ++ [ line ] )


findNotes :
    List String
    -> List ( List String, String )
    -> List ( List String, String )
findNotes block acc =
    case block of
        [ text, meta, _ ] ->
            if text == limitNotice then
                acc

            else if isNote meta then
                case acc of
                    [ ( x, _ ) ] ->
                        [ ( x, text ) ]

                    ( x, _ ) :: xs ->
                        ( x, text ) :: xs

                    _ ->
                        acc

            else
                ( block, "" ) :: acc

        _ ->
            acc


isNote : String -> Bool
isNote =
    startsWith "- Your Note on Page "


limitNotice : String
limitNotice =
    " <You have reached the clipping limit for this item>"


titleAuthorRx : Regex
titleAuthorRx =
    rx "(.+) \\((.+)\\)"


pageRx : Regex
pageRx =
    rx_ " on page (\\d+)"


dateRx : Regex
dateRx =
    rx " \\| Added on \\w+, (\\w+) (\\d+), (\\d+) (\\d+):(\\d+):(\\d+) (\\w+)"


makeDicts : List ( List String, String ) -> ( ExcerptMap, BookMap )
makeDicts =
    foldr
        (\( raw, notes ) ( excerpts, books ) ->
            let
                noOp =
                    ( excerpts, books )
            in
            case raw of
                [ text, meta, titleAuthor ] ->
                    let
                        pair =
                            (if right 1 titleAuthor == ")" then
                                Regex.find titleAuthorRx titleAuthor
                                    |> map .submatches
                                    |> head
                                    |> withDefault []
                                    |> map (withDefault "")

                             else
                                split "-" titleAuthor
                            )
                                |> map trim

                        page =
                            Regex.find pageRx meta
                                |> head
                                |> Maybe.map .submatches
                                |> andThen head
                                |> andThen identity
                                |> andThen toInt

                        dateRaw =
                            case
                                Regex.find dateRx meta
                                    |> head
                                    |> andThen
                                        (.submatches
                                            >> foldr
                                                (Maybe.map2 (::))
                                                (Just [])
                                        )
                            of
                                Just [ month, d, y, h, m, s, meridian ] ->
                                    case map toInt [ y, d, h, m, s ] of
                                        [ Just year, Just day, Just hour, Just minute, Just second ] ->
                                            fromRawParts
                                                { day = day
                                                , month =
                                                    case month of
                                                        "January" ->
                                                            Jan

                                                        "February" ->
                                                            Feb

                                                        "March" ->
                                                            Mar

                                                        "April" ->
                                                            Apr

                                                        "May" ->
                                                            May

                                                        "June" ->
                                                            Jun

                                                        "July" ->
                                                            Jul

                                                        "August" ->
                                                            Aug

                                                        "September" ->
                                                            Sep

                                                        "October" ->
                                                            Oct

                                                        "November" ->
                                                            Nov

                                                        _ ->
                                                            Dec
                                                , year = year
                                                }
                                                { hours =
                                                    hour
                                                        + (if meridian == "AM" then
                                                            0

                                                           else if hour < 12 then
                                                            12

                                                           else
                                                            0
                                                          )
                                                , minutes = minute
                                                , seconds = second
                                                , milliseconds = 0
                                                }
                                                |> Maybe.map
                                                    (toPosix >> posixToMillis)

                                        _ ->
                                            Just 0

                                _ ->
                                    Just 0
                    in
                    case pair of
                        [ titleRaw, authorRaw ] ->
                            let
                                ( excerpt, book ) =
                                    makeExcerpt titleRaw authorRaw text page dateRaw notes
                            in
                            ( insert excerpt.id excerpt excerpts
                            , update excerpt.bookId
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

                        _ ->
                            noOp

                _ ->
                    noOp
        )
        ( Dict.empty, Dict.empty )
