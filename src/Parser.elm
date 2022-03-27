module Parser exposing
    ( getAuthors
    , getBookMap
    , getRouteMap
    , getTags
    , getTitles
    , normalizeTitle
    , process
    )

import Char exposing (isDigit)
import Dict exposing (Dict, get, insert)
import List
    exposing
        ( all
        , concat
        , filterMap
        , foldr
        , head
        , map
        , reverse
        , sortWith
        )
import MD5 exposing (hex)
import Maybe exposing (andThen, withDefault)
import Model exposing (Author, Book, Entry, Id, Tag, Title)
import Regex exposing (Match, Regex, replace)
import Router exposing (slugify)
import Set
import String exposing (lines, repeat, startsWith, toInt, toLower, trim)
import Tuple exposing (first)
import Utils exposing (dedupe, rx)


process : String -> List Entry
process =
    lines
        >> foldr folder ( [], [] )
        >> (\( xs, x ) -> x :: xs)
        >> foldr findNotes []
        >> map makeEntry
        >> filterMap identity
        >> reverse


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
    rx " on page (\\d+)"


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
                split =
                    Regex.find titleAuthorRx titleAuthor
                        |> map .submatches
                        |> head
                        |> withDefault []
                        |> map (withDefault "")
                        |> map trim

                page =
                    Regex.find pageRx meta
                        |> head
                        |> Maybe.map .submatches
                        |> andThen head
                        |> andThen identity
                        |> andThen toInt
            in
            case split of
                [ title, author ] ->
                    Just <|
                        Entry
                            (hex <| text ++ meta)
                            (replace footnoteRx footnoteReplacer text)
                            (replaceApostrophes title)
                            (replaceApostrophes author)
                            page
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


getAuthors : List Entry -> List Author
getAuthors =
    getUniques .author compare


getBookMap : List Entry -> Dict Id Book
getBookMap =
    foldr
        (\{ title, author } ( acc, sortIndex ) ->
            let
                id =
                    hex <| title ++ " " ++ author
            in
            case get id acc of
                Just book ->
                    ( insert id { book | count = .count book + 1 } acc
                    , sortIndex
                    )

                _ ->
                    ( insert id (Book id title author 1 (sortIndex + 1)) acc
                    , sortIndex + 1
                    )
        )
        ( Dict.empty, -1 )
        >> first


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


getRouteMap : List Book -> Dict String Book
getRouteMap =
    map (\book -> ( slugify (.title book), book )) >> Dict.fromList
