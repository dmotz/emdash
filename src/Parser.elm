module Parser exposing
    ( getBooks
    , getRouteMap
    , getTags
    , getTitles
    , normalizeTitle
    , process
    )

import Base64 exposing (fromBytes)
import Bytes.Encode exposing (encode, sequence, unsignedInt8)
import Char exposing (isDigit)
import DateTime exposing (fromRawParts, toPosix)
import Dict exposing (Dict, get, insert)
import List
    exposing
        ( all
        , concat
        , filterMap
        , foldr
        , head
        , map
        , sortWith
        )
import MD5 exposing (bytes)
import Maybe exposing (andThen, withDefault)
import Model exposing (Book, Entry, Id, Tag, Title)
import Regex exposing (Match, Regex, replace)
import Router exposing (slugify)
import Set
import String
    exposing
        ( lines
        , repeat
        , right
        , split
        , startsWith
        , toInt
        , toLower
        , trim
        )
import Time exposing (Month(..), posixToMillis)
import Tuple exposing (first)
import Utils exposing (dedupe, juxt, rx, rx_)


process : String -> Dict Id Entry
process =
    lines
        >> foldr folder ( [], [] )
        >> (\( xs, x ) -> x :: xs)
        >> foldr findNotes []
        >> map makeEntry
        >> filterMap identity
        >> map (juxt .id identity)
        >> Dict.fromList


separator : String
separator =
    repeat 10 "="


hashId : String -> String
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


makeEntry : ( List String, String ) -> Maybe Entry
makeEntry ( raw, notes ) =
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

                date =
                    case
                        Regex.find dateRx meta
                            |> head
                            |> Maybe.map .submatches
                    of
                        Just [ Just month, Just dayRaw, Just yearRaw, Just hourRaw, Just minuteRaw, Just secondRaw, Just meridian ] ->
                            case [ toInt yearRaw, toInt dayRaw, toInt hourRaw, toInt minuteRaw, toInt secondRaw ] of
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
                                        |> Maybe.map (toPosix >> posixToMillis)

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
            in
            case pair of
                [ title, author ] ->
                    Just <|
                        Entry
                            (hashId <| text ++ meta)
                            (replace footnoteRx footnoteReplacer text)
                            (replaceApostrophes title)
                            (replaceApostrophes author)
                            (withDefault 0 date)
                            (withDefault -1 page)
                            []
                            notes

                _ ->
                    Nothing

        _ ->
            Nothing


getUniques :
    (Entry -> String)
    -> (String -> String -> Order)
    -> List Entry
    -> List String
getUniques key sorter entries =
    map key entries
        |> Set.fromList
        |> Set.toList
        |> sortWith sorter


getTitles : List Entry -> List Title
getTitles =
    getUniques .title titleSorter


getBooks : List Entry -> Dict Id Book
getBooks =
    foldr
        (\{ title, author, date } acc ->
            let
                id =
                    hashId <| title ++ " " ++ author
            in
            case get id acc of
                Just book ->
                    insert id
                        { book
                            | count = book.count + 1
                            , sortIndex = max book.sortIndex date
                        }
                        acc

                _ ->
                    insert id (Book id title author 1 date []) acc
        )
        Dict.empty


titleSorter : String -> String -> Order
titleSorter a b =
    compare (normalizeTitle a) (normalizeTitle b)


titlePrefixRx : Regex
titlePrefixRx =
    rx "^(the )"


normalizeTitle : String -> String
normalizeTitle =
    toLower >> replace titlePrefixRx (always "")


getTags : List Entry -> List Tag
getTags =
    map .tags
        >> concat
        >> dedupe


getRouteMap : List Book -> Dict String Id
getRouteMap =
    map (juxt (.title >> slugify) .id) >> Dict.fromList
