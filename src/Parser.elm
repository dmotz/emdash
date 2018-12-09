module Parser exposing (process)

import Bitwise exposing (shiftLeftBy)
import Char exposing (toCode)
import List exposing (filter, foldr, head, map, reverse)
import Maybe exposing (withDefault)
import Model exposing (Entry)
import Regex exposing (Regex)
import String exposing (foldl, lines)


process : String -> List Entry
process raw =
    raw
        |> lines
        |> foldr folder ( [], [] )
        |> Tuple.first
        |> filter predicate
        |> map makeEntry
        |> filter ((/=) Nothing)
        |> map (withDefault <| Entry 0 "" "" "" "")
        |> reverse


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


rx : Regex
rx =
    "(.+) \\((.+)\\)" |> Regex.fromString |> withDefault Regex.never


makeEntry : List String -> Maybe Entry
makeEntry raw =
    case raw of
        [ text, meta, titleAuthor ] ->
            let
                split =
                    Regex.find rx titleAuthor
                        |> map .submatches
                        |> head
                        |> withDefault []
                        |> map (withDefault "")
            in
            case split of
                [ title, author ] ->
                    Just <| Entry (hash text) text title author meta

                _ ->
                    Nothing

        _ ->
            Nothing


hash : String -> Int
hash str =
    foldl updateHash 5381 str


updateHash : Char -> Int -> Int
updateHash c h =
    shiftLeftBy 5 h + h + toCode c
