module Utils exposing
    ( appName
    , countLabel
    , dedupe
    , defaultSemanticThreshold
    , excerptCountLabel
    , findMatches
    , formatNumber
    , formatScore
    , getAuthorRouteMap
    , getAuthors
    , getCount
    , getCounts
    , getExcerptDomId
    , getTagCounts
    , getTitleRouteMap
    , insertOnce
    , juxt
    , makeExcerpt
    , modelToStoredModel
    , null
    , ratingEl
    , removeItem
    , repoUrl
    , rx
    , rx_
    , slugify
    , sortBooks
    , titleCountLabel
    , toDict
    , untaggedKey
    , upsert
    )

import Base64 exposing (fromBytes)
import Bytes.Encode exposing (encode, sequence, unsignedInt8)
import Char exposing (isDigit)
import Dict exposing (Dict, empty, get, insert, member, update, values)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import List
    exposing
        ( all
        , concatMap
        , filter
        , foldl
        , foldr
        , isEmpty
        , length
        , map
        , partition
        , reverse
        , sortBy
        , sortWith
        )
import MD5 exposing (bytes)
import Maybe exposing (withDefault)
import Model exposing (Model)
import Regex exposing (Match, Regex, replace)
import Set
import String exposing (fromInt, join, split, toLower, trim)
import Types
    exposing
        ( Author
        , Book
        , BookMap
        , BookSort(..)
        , CountMap
        , Excerpt
        , Id
        , StoredModel
        , Tag
        )


appName : String
appName =
    "Marginalia"


repoUrl : String
repoUrl =
    "https://github.com/dmotz/marginalia"


untaggedKey : String
untaggedKey =
    "untagged"


defaultSemanticThreshold : Float
defaultSemanticThreshold =
    0.3


inc : Int -> Int
inc =
    (+) 1


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
    { excerpts = values model.excerpts
    , books = values model.books
    , hiddenExcerpts = Set.toList model.hiddenExcerpts
    , bookmarks = Dict.toList model.bookmarks
    , semanticThreshold = model.semanticThreshold
    }


juxt : (a -> b) -> (a -> c) -> a -> ( b, c )
juxt f g x =
    ( f x, g x )


toDict : List { a | id : comparable } -> Dict comparable { a | id : comparable }
toDict =
    map (juxt .id identity) >> Dict.fromList


upsert : Dict comparable a -> comparable -> (a -> a) -> a -> Dict comparable a
upsert dict id f default =
    if member id dict then
        update id (Maybe.map f) dict

    else
        insert id default dict


phraseMatch : Regex -> (a -> String) -> a -> Bool
phraseMatch regex accessor x =
    x |> accessor |> toLower |> Regex.contains regex


findMatches : String -> (a -> String) -> List a -> List a
findMatches query accessor xs =
    let
        ( phraseMatches, rest ) =
            partition (phraseMatch (rx_ <| "\\b" ++ query) accessor) xs

        wordsRx =
            "^"
                ++ (split " " query
                        |> map (\word -> "(?=.*\\b" ++ word ++ ")")
                        |> String.concat
                   )
                ++ ".*$"
                |> rx_
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
            (\tag acc -> update tag (withDefault 0 >> inc >> Just) acc)
            empty
        |> insert untaggedKey (books |> filter (.tags >> isEmpty) |> length)


getExcerptDomId : Id -> String
getExcerptDomId =
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


sortBooks : BookSort -> Bool -> CountMap -> CountMap -> List Book -> List Book
sortBooks sort reverseSort exCounts favCounts =
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
            sortBy <| .id >> getCount exCounts

        RatingSort ->
            sortBy .rating

        FavSort ->
            sortBy <| .id >> getCount favCounts
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


slugify : String -> String
slugify =
    replace (rx "\\s") (always "-")
        >> replace (rx "[^\\w-]") (always "")


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


getBookId : String -> List String -> Id
getBookId title authors =
    hashId (title ++ join " / " authors)


getExcerptId : String -> String -> Int -> Id
getExcerptId text bookId page =
    hashId (text ++ bookId ++ String.fromInt page)


makeExcerpt :
    String
    -> String
    -> String
    -> Maybe Int
    -> Maybe Int
    -> String
    -> Maybe String
    -> ( Excerpt, Book )
makeExcerpt titleRaw authorRaw excerptText mPage mDate notes mUrl =
    let
        title =
            replaceApostrophes titleRaw

        authors =
            authorRaw
                |> replaceApostrophes
                |> Regex.split authorSplitRx
                |> map trim

        page =
            withDefault -1 mPage

        bookId =
            getBookId title authors

        date =
            withDefault 0 mDate
    in
    ( { id = getExcerptId excerptText bookId page
      , text = replace footnoteRx footnoteReplacer excerptText
      , bookId = bookId
      , date = date
      , page = page
      , notes = notes
      , isFavorite = False
      , sourceUrl = mUrl
      }
    , { id = bookId
      , title = title
      , authors = authors
      , rating = 0
      , sortIndex = date
      , tags = []
      , slug = ""
      }
    )


getAuthors : Model -> List ( Id, List Id )
getAuthors model =
    model.excerpts
        |> values
        |> foldl
            (\excerpt acc ->
                foldl
                    (\author acc2 ->
                        insert
                            author
                            (excerpt.id
                                :: withDefault []
                                    (get author acc2)
                            )
                            acc2
                    )
                    acc
                    (withDefault
                        []
                        (get excerpt.bookId model.books
                            |> Maybe.map .authors
                        )
                    )
            )
            Dict.empty
        |> Dict.toList


getCounts : List Excerpt -> ( CountMap, CountMap )
getCounts =
    foldr
        (\{ bookId, isFavorite } ( exCount, favCount ) ->
            ( upsert exCount bookId inc 1
            , if isFavorite then
                upsert favCount bookId inc 1

              else
                favCount
            )
        )
        ( Dict.empty, Dict.empty )


getCount : CountMap -> Id -> Int
getCount dict id =
    get id dict |> withDefault 0
