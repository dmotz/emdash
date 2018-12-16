module Parser exposing (process)

import Bitwise exposing (shiftLeftBy)
import Char exposing (toCode)
import List exposing (filter, foldr, head, map, reverse)
import Maybe exposing (andThen, withDefault)
import Model exposing (Entry)
import Regex exposing (Regex)
import String exposing (foldl, lines, toInt)


process : String -> List Entry
process =
    lines
        >> foldr folder ( [], [] )
        >> Tuple.first
        >> filter predicate
        >> map makeEntry
        >> filter ((/=) Nothing)
        >> map (withDefault <| Entry 0 "" "" "" "" Nothing)
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
    "(.+) \\((.+)\\)" |> Regex.fromString |> withDefault Regex.never


pageRx : Regex
pageRx =
    " on page (\\d+)" |> Regex.fromString |> withDefault Regex.never


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
                    Just <| Entry (hash text) text title author meta page

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
