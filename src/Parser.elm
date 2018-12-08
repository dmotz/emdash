module Parser exposing (process)

import List exposing (head, map)
import Maybe exposing (withDefault)
import Model exposing (Entry)
import Regex


process : String -> List Entry
process raw =
    raw
        |> String.lines
        |> List.foldr folder ( [], [] )
        |> Tuple.first
        |> List.filter predicate
        |> List.map makeEntry
        |> List.filter ((/=) Nothing)
        |> List.map (withDefault <| Entry "" "" "" "")


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


rx : Regex.Regex
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
                    Just <| Entry text title author meta

                _ ->
                    Nothing

        _ ->
            Nothing
