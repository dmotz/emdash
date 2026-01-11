module KindleParser exposing (parse)

import DateTime exposing (fromRawParts, toPosix)
import Dict
import List exposing (drop, filter, filterMap, head, indexedMap, reverse, take)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , backtrackable
        , chompIf
        , chompUntil
        , chompWhile
        , deadEndsToString
        , end
        , getChompedString
        , int
        , loop
        , map
        , oneOf
        , problem
        , run
        , spaces
        , succeed
        , symbol
        )
import String exposing (trim)
import Time exposing (Month(..), posixToMillis)
import Types exposing (BookMap, ExcerptMap)
import Utils exposing (makeExcerpt)


type alias Metadata =
    { type_ : MetadataType
    , page : Maybe Int
    , location : Maybe Int
    , date : Maybe Int
    }


type MetadataType
    = Highlight
    | Bookmark
    | Note
    | Unknown


separator : String
separator =
    "=========="


parse : String -> Result String ( ExcerptMap, BookMap )
parse input =
    case run (loop ( [], Dict.empty, Dict.empty ) blockLoop) input of
        Ok ( _, excerpts, books ) ->
            Ok ( excerpts, books )

        Err e ->
            Err <| "Failed to parse Kindle highlights: " ++ deadEndsToString e


type alias State =
    ( List String, ExcerptMap, BookMap )


blockLoop : State -> Parser (Step State State)
blockLoop ( lastIds, excerpts, books ) =
    oneOf
        [ succeed
            (\entry ->
                Loop (updateState entry ( lastIds, excerpts, books ))
            )
            |= entryParser
        , succeed (Done ( lastIds, excerpts, books ))
            |. end
        ]


updateState : Maybe Entry -> State -> State
updateState mEntry ( lastIds, excerpts, books ) =
    case mEntry of
        Just (HighlightEntry e) ->
            let
                ( excerpt, book ) =
                    makeExcerpt
                        e.title
                        e.author
                        e.content
                        e.page
                        e.date
                        ""
                        Nothing

                newExcerpts =
                    Dict.insert excerpt.id excerpt excerpts

                newBooks =
                    Dict.update book.id
                        (\mBook ->
                            Just <|
                                case mBook of
                                    Just b ->
                                        { b
                                            | sortIndex =
                                                max b.sortIndex excerpt.date
                                        }

                                    Nothing ->
                                        book
                        )
                        books
            in
            ( excerpt.id :: lastIds, newExcerpts, newBooks )

        Just (NoteEntry n) ->
            case lastIds of
                id :: _ ->
                    let
                        newExcerpts =
                            Dict.update
                                id
                                (Maybe.map (\e -> { e | notes = n.content }))
                                excerpts
                    in
                    ( lastIds, newExcerpts, books )

                [] ->
                    ( lastIds, excerpts, books )

        Nothing ->
            ( lastIds, excerpts, books )


type Entry
    = HighlightEntry
        { title : String
        , author : String
        , content : String
        , page : Maybe Int
        , date : Maybe Int
        }
    | NoteEntry
        { title : String
        , author : String
        , content : String
        , page : Maybe Int
        , date : Maybe Int
        }


entryParser : Parser (Maybe Entry)
entryParser =
    succeed
        (\( title, author ) meta content ->
            let
                page =
                    case meta.page of
                        Just p ->
                            Just p

                        Nothing ->
                            Maybe.map (max 1 << (//) 15) meta.location
            in
            case meta.type_ of
                Highlight ->
                    Just
                        (HighlightEntry
                            { title = title
                            , author = author
                            , content = content
                            , page = page
                            , date = meta.date
                            }
                        )

                Note ->
                    Just
                        (NoteEntry
                            { title = title
                            , author = author
                            , content = content
                            , page = page
                            , date = meta.date
                            }
                        )

                _ ->
                    Nothing
        )
        |= titleAuthorParser
        |. lineBreak
        |= metadataParser
        |. lineBreak
        |. spaces
        |= contentParser
        |. symbol separator
        |. spaces


lineBreak : Parser ()
lineBreak =
    oneOf
        [ symbol "\u{000D}\n"
        , symbol "\n"
        ]


titleAuthorParser : Parser ( String, String )
titleAuthorParser =
    getChompedString (chompUntil "\n") |> map splitTitleAuthor


splitTitleAuthor : String -> ( String, String )
splitTitleAuthor s =
    let
        trimmed =
            trim s
    in
    if String.endsWith ")" trimmed then
        case String.indices "(" trimmed |> reverse |> head of
            Just openParenIndex ->
                ( trim (String.left openParenIndex trimmed)
                , trim
                    (String.slice
                        (openParenIndex + 1)
                        (String.length trimmed - 1)
                        trimmed
                    )
                )

            Nothing ->
                splitByDash trimmed

    else
        splitByDash trimmed


splitByDash : String -> ( String, String )
splitByDash s =
    case String.split " - " s of
        [ t, a ] ->
            ( trim t, trim a )

        _ ->
            ( trim s, "" )


metadataParser : Parser Metadata
metadataParser =
    getChompedString (chompUntil "\n")
        |> andThen
            (\line ->
                case run subMetadataParser (trim line) of
                    Ok m ->
                        succeed m

                    Err _ ->
                        succeed
                            { type_ = Unknown
                            , page = Nothing
                            , location = Nothing
                            , date = Nothing
                            }
            )


subMetadataParser : Parser Metadata
subMetadataParser =
    succeed
        (\type_ mPage mLocation date ->
            { type_ = type_
            , page = mPage
            , location = mLocation
            , date = date
            }
        )
        |. symbol "- Your "
        |= typeParser
        |= oneOf
            [ backtrackable
                (succeed Just
                    |. oneOf [ chompUntil "Page ", chompUntil "page " ]
                    |. oneOf [ symbol "Page ", symbol "page " ]
                    |= int
                )
            , succeed Nothing
            ]
        |= oneOf
            [ backtrackable
                (succeed Just
                    |. oneOf [ chompUntil "Location ", chompUntil "location " ]
                    |. oneOf [ symbol "Location ", symbol "location " ]
                    |= int
                    |. chompWhile (\c -> Char.isDigit c || c == '-')
                )
            , succeed Nothing
            ]
        |= oneOf
            [ backtrackable
                (succeed
                    identity
                    |. chompUntil "Added on "
                    |. symbol "Added on "
                    |= dateParser
                )
            , succeed Nothing
            ]


typeParser : Parser MetadataType
typeParser =
    oneOf
        [ symbol "Highlight" |> map (always Highlight)
        , symbol "Bookmark" |> map (always Bookmark)
        , symbol "Note" |> map (always Note)
        , succeed Unknown
        ]


monthToEnum : String -> Maybe Month
monthToEnum s =
    case String.toLower (trim s) of
        "january" ->
            Just Jan

        "february" ->
            Just Feb

        "march" ->
            Just Mar

        "april" ->
            Just Apr

        "may" ->
            Just May

        "june" ->
            Just Jun

        "july" ->
            Just Jul

        "august" ->
            Just Aug

        "september" ->
            Just Sep

        "october" ->
            Just Oct

        "november" ->
            Just Nov

        "december" ->
            Just Dec

        _ ->
            Nothing


monthFromInt : Int -> Month
monthFromInt n =
    case n of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        12 ->
            Dec

        _ ->
            Jan


dateParser : Parser (Maybe Int)
dateParser =
    loop ( [], [] )
        (\( nums, words ) ->
            oneOf
                [ backtrackable
                    (getChompedString (chompWhile Char.isDigit)
                        |> andThen
                            (\s ->
                                if s == "" then
                                    problem "not a number"

                                else
                                    case String.toInt s of
                                        Just n ->
                                            succeed (Loop ( n :: nums, words ))

                                        Nothing ->
                                            problem "not a number"
                            )
                    )
                , backtrackable
                    (getChompedString (chompWhile Char.isAlpha)
                        |> andThen
                            (\s ->
                                if s == "" then
                                    problem "empty"

                                else
                                    succeed (Loop ( nums, s :: words ))
                            )
                    )
                , chompIf (always True) |> map (\_ -> Loop ( nums, words ))
                , succeed (Done ( reverse nums, reverse words ))
                ]
        )
        |> map
            (\( nums, words ) ->
                let
                    yearMatch =
                        nums
                            |> indexedMap (\i n -> ( i, n ))
                            |> filter (\( _, n ) -> n > 1900)
                            |> head

                    ( year, dateNums, timeNums ) =
                        case yearMatch of
                            Just ( idx, y ) ->
                                ( y
                                , take idx nums
                                , drop (idx + 1) nums
                                )

                            Nothing ->
                                ( 2023, [], nums )

                    monthFromWords =
                        filterMap monthToEnum words |> head

                    day =
                        case ( monthFromWords, dateNums ) of
                            ( Just _, dVal :: _ ) ->
                                dVal

                            ( Nothing, _ :: dVal :: _ ) ->
                                dVal

                            ( _, dVal :: _ ) ->
                                dVal

                            _ ->
                                1

                    month =
                        case ( monthFromWords, dateNums ) of
                            ( Just m, _ ) ->
                                m

                            ( Nothing, mVal :: _ ) ->
                                monthFromInt mVal

                            _ ->
                                Jan

                    ( h, mi, s ) =
                        case timeNums of
                            h1 :: m1 :: s1 :: _ ->
                                ( h1, m1, s1 )

                            h1 :: m1 :: _ ->
                                ( h1, m1, 0 )

                            h1 :: _ ->
                                ( h1, 0, 0 )

                            _ ->
                                ( 0, 0, 0 )

                    meridian =
                        filter
                            (\w ->
                                let
                                    lw =
                                        String.toLower w
                                in
                                lw == "am" || lw == "pm"
                            )
                            words
                            |> head

                    hour =
                        case meridian |> Maybe.map String.toLower of
                            Just "pm" ->
                                if h < 12 then
                                    h + 12

                                else
                                    h

                            Just "am" ->
                                if h == 12 then
                                    0

                                else
                                    h

                            _ ->
                                h
                in
                fromRawParts
                    { day = day
                    , month = month
                    , year = year
                    }
                    { hours = hour, minutes = mi, seconds = s, milliseconds = 0 }
                    |> Maybe.map (toPosix >> posixToMillis)
            )


contentParser : Parser String
contentParser =
    separator |> chompUntil |> getChompedString |> map trim
