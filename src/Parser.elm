module Parser exposing (getAuthors, getTags, getTitles, process)

import List exposing (concat, filter, foldr, head, map, reverse, sortWith)
import MD5 exposing (hex)
import Maybe exposing (andThen, withDefault)
import Model exposing (Author, Entry, Tag, Title)
import Regex exposing (Regex)
import Set
import String exposing (lines, toInt, toLower, trim)
import Utils exposing (rx)


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


titleAuthorRx : Regex
titleAuthorRx =
    rx "(.+) \\((.+)\\)"


pageRx : Regex
pageRx =
    rx " on page (\\d+)"


footnoteRx : Regex
footnoteRx =
    rx "\\.(\\d+) "


apostropheRx : Regex
apostropheRx =
    rx "(\\w)(')(\\w)"


apostropheReplacer : Regex.Match -> String
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
                            (Regex.replace
                                apostropheRx
                                apostropheReplacer
                                title
                            )
                            (Regex.replace
                                apostropheRx
                                apostropheReplacer
                                author
                            )
                            page
                            []

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


titleSorter : String -> String -> Order
titleSorter a b =
    compare (normalizeTitle a) (normalizeTitle b)


titlePrefixRx : Regex
titlePrefixRx =
    rx "^(the )"


normalizeTitle : String -> String
normalizeTitle =
    toLower >> Regex.replace titlePrefixRx (always "")


getTags : List Entry -> List Tag
getTags =
    map .tags
        >> concat
        >> Set.fromList
        >> Set.toList
