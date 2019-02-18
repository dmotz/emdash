module Parser exposing (getTitles, process)

import List exposing (filter, foldr, head, map, reverse, sortWith)
import MD5 exposing (hex)
import Maybe exposing (andThen, withDefault)
import Model exposing (Book, Entry)
import Regex exposing (Regex)
import Set
import String exposing (lines, toInt, toLower, trim)


process : String -> List Entry
process =
    lines
        >> foldr folder ( [], [] )
        >> Tuple.first
        >> filter predicate
        >> map makeEntry
        >> filter ((/=) Nothing)
        >> map (withDefault <| Entry "" "" "" "" Nothing [])
        >> reverse


folder :
    String
    -> ( List (List String), List String )
    -> ( List (List String), List String )
folder line ( blocks, currentBlock ) =
    if line == "" then
        ( blocks, currentBlock )

    else if line == "==========" then
        ( currentBlock :: blocks, [] )

    else
        ( blocks, currentBlock ++ [ line ] )


predicate : List String -> Bool
predicate block =
    if block == [] then
        False

    else
        case head block of
            Nothing ->
                False

            Just line ->
                not <| String.startsWith "- Your Bookmark" line


rx : String -> Regex
rx =
    Regex.fromString >> withDefault Regex.never


titleAuthorRx : Regex
titleAuthorRx =
    rx "(.+) \\((.+)\\)"


pageRx : Regex
pageRx =
    rx " on page (\\d+)"


footnoteRx : Regex
footnoteRx =
    rx "\\.(\\d+) "


makeEntry : List String -> Maybe Entry
makeEntry raw =
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
                        |> andThen String.toInt
            in
            case split of
                [ title, author ] ->
                    Just <|
                        Entry
                            (hex <| text ++ meta)
                            (Regex.replace footnoteRx (always ".") text)
                            title
                            author
                            page
                            []

                _ ->
                    Nothing

        _ ->
            Nothing


getTitles : List Entry -> List Book
getTitles =
    map .title
        >> Set.fromList
        >> Set.toList
        >> sortWith titleSorter


titleSorter : String -> String -> Order
titleSorter a b =
    compare (normalizeTitle a) (normalizeTitle b)


titlePrefixRx : Regex
titlePrefixRx =
    rx "^(the )"


normalizeTitle : String -> String
normalizeTitle =
    toLower >> Regex.replace titlePrefixRx (always "")
