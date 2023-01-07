module Parser exposing (getAuthorRouteMap, getExcerptId, getTitleRouteMap, process)

import Base64 exposing (fromBytes)
import Bytes.Encode exposing (encode, sequence, unsignedInt8)
import Char exposing (isDigit)
import DateTime exposing (fromRawParts, toPosix)
import Dict exposing (Dict, get, insert, update)
import List exposing (all, concatMap, foldr, head, map, reverse, sortBy)
import MD5 exposing (bytes)
import Maybe exposing (andThen, withDefault)
import Model exposing (Author, Book, BookMap, ExcerptMap, Id)
import Regex exposing (Match, Regex, replace)
import Router exposing (slugify)
import String exposing (join, lines, repeat, right, split, startsWith, toInt, trim)
import Time exposing (Month(..), posixToMillis)
import Utils exposing (juxt, rx, rx_)


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


hashId : String -> Id
hashId =
    bytes
        >> map unsignedInt8
        >> sequence
        >> encode
        >> fromBytes
        >> withDefault ""
        >> String.replace "==" ""
        >> String.replace "+" "-"
        >> String.replace "/" "_"


getExcerptId : String -> String -> Id
getExcerptId text meta =
    (text ++ meta) |> hashId


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


footnoteRx : Regex
footnoteRx =
    rx "([^\\s\\d]{2,})(\\d+)"


footnoteReplacer : Match -> String
footnoteReplacer match =
    String.concat <|
        map
            (\sub ->
                let
                    s =
                        withDefault "" sub
                in
                if all isDigit (String.toList s) then
                    ""

                else
                    s
            )
            match.submatches


apostropheRx : Regex
apostropheRx =
    rx "(\\w)(')(\\w)"


apostropheReplacer : Match -> String
apostropheReplacer match =
    String.concat <|
        map
            (\sub ->
                let
                    s =
                        withDefault "" sub
                in
                if s == "'" then
                    "’"

                else
                    s
            )
            match.submatches


replaceApostrophes : String -> String
replaceApostrophes =
    replace apostropheRx apostropheReplacer


authorSplitRx : Regex
authorSplitRx =
    rx "[;&]|\\sand\\s"


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
                                id =
                                    getExcerptId text meta

                                title =
                                    replaceApostrophes titleRaw

                                authors =
                                    authorRaw
                                        |> replaceApostrophes
                                        |> Regex.split authorSplitRx
                                        |> map trim

                                bookId =
                                    hashId <| title ++ " " ++ authorRaw

                                date =
                                    withDefault 0 dateRaw
                            in
                            ( insert
                                id
                                { id = id
                                , text = replace footnoteRx footnoteReplacer text
                                , bookId = bookId
                                , date = date
                                , page = withDefault -1 page
                                , notes = notes
                                , isFavorite = False
                                }
                                excerpts
                            , update bookId
                                (\mBook ->
                                    Just <|
                                        case mBook of
                                            Just book ->
                                                { book
                                                    | sortIndex =
                                                        max
                                                            book.sortIndex
                                                            date
                                                }

                                            _ ->
                                                { id = bookId
                                                , title = title
                                                , authors = authors
                                                , count = 0
                                                , rating = 0
                                                , sortIndex = date
                                                , tags = []
                                                , slug = ""
                                                , favCount = 0
                                                }
                                )
                                books
                            )

                        _ ->
                            noOp

                _ ->
                    noOp
        )
        ( Dict.empty, Dict.empty )


getTitleRouteMap : List Book -> ( Dict String Id, List Book )
getTitleRouteMap =
    sortBy .sortIndex
        >> reverse
        >> foldr
            (\book ( slugToId, newBooks ) ->
                let
                    slug =
                        case get (slugify book.title) slugToId of
                            Just _ ->
                                slugify
                                    (book.title
                                        ++ " by "
                                        ++ join " & " book.authors
                                    )

                            _ ->
                                slugify book.title
                in
                ( insert slug book.id slugToId
                , { book | slug = slug } :: newBooks
                )
            )
            ( Dict.empty, [] )


getAuthorRouteMap : List Book -> Dict String Author
getAuthorRouteMap =
    concatMap (.authors >> map (juxt slugify identity)) >> Dict.fromList
