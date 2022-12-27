module Views.BookList exposing (bookList, bookView)

import Html exposing (Html, a, div, img, li, span, text)
import Html.Attributes exposing (class, href, src)
import Html.Keyed as Keyed
import List exposing (map)
import Model exposing (Book, BookSort(..))
import Msg exposing (Msg)
import Router exposing (titleSlugToRoute)
import String exposing (fromInt, join)
import Utils exposing (null, ratingEl, sortBooks)


bookList : List Book -> BookSort -> Bool -> Html Msg
bookList books sort reverseSort =
    let
        showRating =
            sort == RatingSort

        showFavCount =
            sort == FavSort
    in
    Keyed.ul
        [ class "bookList" ]
        (map
            (\book -> ( book.id, bookView li book showRating showFavCount ))
            (sortBooks sort reverseSort books)
        )


bookView :
    (List (Html.Attribute msg) -> List (Html Msg) -> Html Msg)
    -> Book
    -> Bool
    -> Bool
    -> Html Msg
bookView tag book showRating showFavCount =
    tag
        [ class "book" ]
        [ a
            [ href <| titleSlugToRoute book.slug ]
            [ div [ class "title" ] [ text book.title ]
            , div [ class "author" ] [ text <| join " / " book.authors ]
            , div [ class "count" ] [ text <| fromInt book.count ]
            , if showRating then
                ratingEl book

              else
                null
            , if showFavCount then
                div
                    [ class "favCount" ]
                    (if book.favCount > 0 then
                        [ img
                            [ class "favIcon", src "/images/favorite-filled.svg" ]
                            []
                        , text <| fromInt book.favCount
                        ]

                     else
                        [ span [ class "unrated" ] [ text "—" ] ]
                    )

              else
                null
            ]
        ]
