module Views.BookList exposing (bookList, bookView)

import Html exposing (Html, a, div, li, text)
import Html.Attributes exposing (class, href)
import Html.Keyed as Keyed
import List exposing (map)
import Model exposing (Book, BookSort(..))
import Msg exposing (Msg)
import Router exposing (titleSlugToRoute)
import String exposing (fromInt)
import Utils exposing (null, ratingStr, sortBooks)


bookList : List Book -> BookSort -> Bool -> Html Msg
bookList books sort reverseSort =
    let
        showRating =
            sort == RatingSort
    in
    Keyed.ul
        [ class "bookList" ]
        (map
            (\book -> ( book.id, bookView li book showRating ))
            (sortBooks sort reverseSort books)
        )


bookView :
    (List (Html.Attribute msg) -> List (Html Msg) -> Html Msg)
    -> Book
    -> Bool
    -> Html Msg
bookView tag book showRating =
    tag
        [ class "book" ]
        [ a
            [ href <| titleSlugToRoute book.slug ]
            [ div [ class "title" ] [ text book.title ]
            , div [ class "author" ] [ text book.author ]
            , div [ class "count" ] [ text <| fromInt book.count ]
            , if showRating then
                div [ class "rating" ] [ text <| ratingStr book ]

              else
                null
            ]
        ]
