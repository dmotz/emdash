module Views.BookList exposing (bookList, bookView)

import Dict exposing (Dict, get)
import Html exposing (Html, a, div, img, li, span, text)
import Html.Attributes exposing (class, href, src)
import Html.Keyed as Keyed
import List exposing (map)
import Maybe exposing (withDefault)
import Msg exposing (Msg)
import Router exposing (titleSlugToRoute)
import String exposing (fromInt, join)
import Types exposing (Book, BookSort(..), Id)
import Utils exposing (null, ratingEl, sortBooks)


bookList : List Book -> Dict Id Int -> Dict Id Int -> BookSort -> Bool -> Html Msg
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
                    (get book.id exCounts |> withDefault 0)
                    (get book.id favCounts |> withDefault 0)
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
            [ href <|
                if isLandingPage then
                    "#"

                else
                    titleSlugToRoute book.slug
            ]
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
