module Views.BookList exposing (bookList, bookView)

import Html exposing (Html, a, div, img, li, span, text)
import Html.Attributes exposing (alt, class, href, src, tabindex)
import Html.Keyed as Keyed
import List exposing (map)
import Msg exposing (Msg)
import Router exposing (titleSlugToRoute)
import String exposing (fromInt, join)
import Types exposing (Book, BookSort(..), CountMap)
import Utils exposing (getCount, null, ratingEl, sortBooks)


bookList : List Book -> CountMap -> CountMap -> BookSort -> Bool -> Html Msg
bookList books exCounts favCounts sort reverseSort =
    let
        showRating =
            sort == RatingSort

        showFavCount =
            sort == FavSort
    in
    Keyed.ul
        [ class "bookList" ]
        (map
            (\book ->
                ( book.id
                , bookView
                    book
                    (getCount exCounts book.id)
                    (getCount favCounts book.id)
                    showRating
                    showFavCount
                    False
                )
            )
            (sortBooks sort reverseSort exCounts favCounts books)
        )


bookView : Book -> Int -> Int -> Bool -> Bool -> Bool -> Html Msg
bookView book exCount favCount showRating showFavCount isLandingPage =
    (if isLandingPage then
        div

     else
        li
    )
        [ class "book" ]
        [ a
            (if isLandingPage then
                [ href "#", tabindex -1 ]

             else
                [ href <| titleSlugToRoute book.slug ]
            )
            [ div [ class "title" ] [ text book.title ]
            , div [ class "author" ] [ text <| join " / " book.authors ]
            , div [ class "count" ] [ text <| fromInt exCount ]
            , if showRating then
                ratingEl book

              else
                null
            , if showFavCount then
                div
                    [ class "favCount" ]
                    (if favCount > 0 then
                        [ img
                            [ class "icon"
                            , src "/images/icons/favorite.svg"
                            , alt "favorites"
                            ]
                            []
                        , text <| fromInt favCount
                        ]

                     else
                        [ span [ class "unrated" ] [ text "—" ] ]
                    )

              else
                null
            ]
        ]
