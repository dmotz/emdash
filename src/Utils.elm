module Utils exposing
    ( ClickWithKeys
    , KeyEvent
    , charLimit
    , dedupe
    , embeddingBatchSize
    , find
    , findMatches
    , formatNumber
    , getIndex
    , getNextIndex
    , getPrevIndex
    , getTagCounts
    , insertOnce
    , juxt
    , modelToStoredModel
    , pluckIds
    , queryCharMin
    , removeItem
    , rx
    , rx_
    , takeExcerpt
    , toDict
    , untaggedKey
    , updateItem
    , updateItems
    )

import Dict exposing (Dict, empty, get, insert, update, values)
import List exposing (concatMap, filter, filterMap, foldl, foldr, isEmpty, length, map, partition)
import Maybe exposing (withDefault)
import Model exposing (BookMap, Filter(..), Id, Model, StoredModel, Tag)
import Regex exposing (Regex, replace)
import Set
import String exposing (fromChar, fromInt, join, split, toList, toLower)


type alias KeyEvent =
    { key : String
    , control : Bool
    , meta : Bool
    }


type alias ClickWithKeys =
    { control : Bool
    , meta : Bool
    , shift : Bool
    }


queryCharMin : Int
queryCharMin =
    4


untaggedKey : String
untaggedKey =
    "untagged"


rx : String -> Regex
rx =
    Regex.fromString >> withDefault Regex.never


rx_ : String -> Regex
rx_ =
    Regex.fromStringWith { caseInsensitive = True, multiline = False }
        >> withDefault Regex.never


formatNumber : Int -> String
formatNumber =
    fromInt >> replace (rx "\\B(?=(\\d{3})+(?!\\d))") (always ",")


insertOnce : List comparable -> comparable -> List comparable
insertOnce list x =
    Set.toList <| Set.insert x (Set.fromList list)


removeItem : List comparable -> comparable -> List comparable
removeItem list x =
    Set.toList <| Set.remove x (Set.fromList list)


updateItem : List a -> a -> a -> List a
updateItem list old new =
    map
        (\x ->
            if x == old then
                new

            else
                x
        )
        list


updateItems :
    List { a | id : comparable }
    -> Dict comparable { a | id : comparable }
    -> List { a | id : comparable }
updateItems list mapping =
    map
        (\x ->
            withDefault x (get x.id mapping)
        )
        list


getIndex : List a -> a -> Int
getIndex list item =
    let
        f l target n =
            case l of
                [] ->
                    -1

                x :: xs ->
                    if x == target then
                        n

                    else
                        f xs target (n + 1)
    in
    f list item 0


getNextIndex : List a -> a -> Int
getNextIndex list item =
    let
        idx =
            getIndex list item + 1
    in
    if idx == length list then
        0

    else
        idx


getPrevIndex : List a -> a -> Int
getPrevIndex list item =
    let
        idx =
            getIndex list item
    in
    if idx == 0 then
        length list - 1

    else
        idx - 1


dedupe : List comparable -> List comparable
dedupe =
    Set.fromList >> Set.toList


modelToStoredModel : Model -> StoredModel
modelToStoredModel model =
    { entries = values model.entries
    , books = values model.books
    , hiddenEntries = Set.toList model.hiddenEntries
    , focusMode = model.focusMode
    , reverseSort = model.reverseSort
    , schemaVersion = model.schemaVersion
    , bookIdToLastRead = Dict.toList model.bookIdToLastRead
    }


charLimit : Int
charLimit =
    42


embeddingBatchSize : Int
embeddingBatchSize =
    10


takeExcerpt : String -> String
takeExcerpt text =
    let
        f acc chars n =
            case chars of
                x :: xs ->
                    if n < charLimit || n >= charLimit && x /= ' ' then
                        f (acc ++ fromChar x) xs (n + 1)

                    else
                        acc

                [] ->
                    acc
    in
    f "" (toList text) 0 ++ " …"


find : List a -> (a -> Bool) -> Maybe a
find l f =
    case l of
        x :: xs ->
            if f x then
                Just x

            else
                find xs f

        [] ->
            Nothing


juxt : (a -> b) -> (a -> c) -> a -> ( b, c )
juxt f g x =
    ( f x, g x )


pluckIds : Dict Id a -> List Id -> List a
pluckIds xs ids =
    filterMap (\id -> get id xs) ids


toDict : List { a | id : comparable } -> Dict comparable { a | id : comparable }
toDict =
    map (juxt .id identity) >> Dict.fromList


phraseMatch : Regex -> (a -> String) -> a -> Bool
phraseMatch regex accessor x =
    x |> accessor |> toLower |> Regex.contains regex


findMatches : String -> (a -> String) -> List a -> List a
findMatches query accessor xs =
    let
        ( phraseMatches, rest ) =
            partition (phraseMatch (rx <| "\\b" ++ query) accessor) xs

        pattern =
            split " " query
                |> map (\word -> "(?=.*\\b" ++ word ++ ")")
                |> join ""

        wordsRx =
            "^" ++ pattern ++ ".*$" |> rx
    in
    phraseMatches
        ++ filter
            (\x -> Regex.contains wordsRx (toLower (accessor x)))
            rest


getTagCounts : BookMap -> Dict Tag Int
getTagCounts bookMap =
    let
        books =
            values bookMap
    in
    books
        |> concatMap .tags
        |> foldl
            (\tag acc -> update tag (withDefault 0 >> (+) 1 >> Just) acc)
            empty
        |> insert
            untaggedKey
            (filter (\{ tags } -> isEmpty tags) books |> length)
