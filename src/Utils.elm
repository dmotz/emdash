module Utils exposing
    ( KeyEvent
    , dedupe
    , excerptCountLabel
    , findMatches
    , formatNumber
    , formatScore
    , getEntryDomId
    , getTagCounts
    , insertOnce
    , juxt
    , modelToStoredModel
    , null
    , ratingEl
    , removeItem
    , rx
    , rx_
    , sortBooks
    , titleCountLabel
    , untaggedKey
    )

import Dict exposing (Dict, empty, insert, update, values)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import List
    exposing
        ( concatMap
        , filter
        , foldl
        , isEmpty
        , length
        , map
        , partition
        , reverse
        , sortBy
        , sortWith
        )
import Maybe exposing (withDefault)
import Model
    exposing
        ( Book
        , BookMap
        , BookSort(..)
        , Id
        , Model
        , StoredModel
        , Tag
        )
import Regex exposing (Regex, replace)
import Set
import String exposing (fromInt, split, toLower)


type alias KeyEvent =
    { key : String
    , control : Bool
    , meta : Bool
    }


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


dedupe : List comparable -> List comparable
dedupe =
    Set.fromList >> Set.toList


modelToStoredModel : Model -> StoredModel
modelToStoredModel model =
    { entries = values model.entries
    , books = values model.books
    , hiddenEntries = Set.toList model.hiddenEntries
    , bookmarks = Dict.toList model.bookmarks
    }


juxt : (a -> b) -> (a -> c) -> a -> ( b, c )
juxt f g x =
    ( f x, g x )


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
                |> String.concat

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


getEntryDomId : Id -> String
getEntryDomId =
    (++) "excerpt-"


countLabel : String -> Int -> String
countLabel label n =
    formatNumber n
        ++ " "
        ++ label
        ++ (if n == 1 then
                ""

            else
                "s"
           )


formatScore : Float -> Html msg
formatScore =
    (*) 100 >> round >> fromInt >> (\s -> s ++ "%") >> text


excerptCountLabel : Int -> String
excerptCountLabel =
    countLabel "excerpt"


titleCountLabel : Int -> String
titleCountLabel =
    countLabel "title"


normalizeTitle : String -> String
normalizeTitle =
    toLower >> replace (rx "^(the )") (always "")


sortBooks : BookSort -> Bool -> List Book -> List Book
sortBooks sort reverseSort =
    (case sort of
        RecencySort ->
            sortBy .sortIndex

        TitleSort ->
            sortWith
                (\a b ->
                    compare
                        (a |> .title |> normalizeTitle)
                        (b |> .title |> normalizeTitle)
                )

        NumSort ->
            sortBy .count

        RatingSort ->
            sortBy .rating

        FavSort ->
            sortBy .favCount
    )
        >> (if reverseSort then
                reverse

            else
                identity
           )


ratingEl : Book -> Html msg
ratingEl book =
    let
        baseInt =
            truncate book.rating
    in
    div
        [ classList [ ( "ratingNum", True ), ( "unrated", book.rating == 0 ) ] ]
        (if book.rating == 0 then
            [ text "—" ]

         else if ceiling book.rating > baseInt then
            [ if baseInt == 0 then
                null

              else
                book.rating |> truncate |> fromInt |> text
            , span [ class "half" ] [ text "1/2" ]
            ]

         else
            [ text (String.fromFloat book.rating) ]
        )


null : Html msg
null =
    text ""
