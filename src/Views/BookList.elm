module Views.BookList exposing (book, bookList)

import Html exposing (Html, a, div, li, text)
import Html.Attributes exposing (class, href)
import Html.Keyed as Keyed
import List exposing (map)
import Model exposing (Author, Book, BookSort, Title)
import Msg exposing (Msg)
import Router exposing (titleToRoute)
import String exposing (fromInt)
import Utils exposing (sortBooks)


bookList : List Book -> BookSort -> Bool -> Html Msg
bookList books sort reverseSort =
    Keyed.ul
        [ class "bookList" ]
        (map
            (\{ id, title, author, count } -> ( id, book title author count ))
            (sortBooks sort reverseSort books)
        )


book : Title -> Author -> Int -> Html Msg
book title author count =
    li
        [ class "book" ]
        [ a
            [ href <| titleToRoute title ]
            [ div [ class "title" ] [ text title ]
            , div [ class "author" ] [ text author ]
            , div [ class "count" ] [ text <| fromInt count ]
            ]
        ]
