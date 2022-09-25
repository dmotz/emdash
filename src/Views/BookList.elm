module Views.BookList exposing (bookList)

import Html exposing (Html, a, div, li, text)
import Html.Attributes exposing (class, href)
import Html.Keyed as Keyed
import List exposing (map)
import Model exposing (Book, BookSort)
import Msg exposing (Msg)
import Router exposing (titleToRoute)
import String exposing (fromInt)
import Utils exposing (sortBooks)


bookList : List Book -> BookSort -> Bool -> Html Msg
bookList books sort reverseSort =
    Keyed.ul
        [ class "bookList" ]
        (map
            (\{ id, title, author, count } ->
                ( id
                , li []
                    [ a
                        [ href <| titleToRoute title ]
                        [ div [ class "title" ] [ text title ]
                        , div [ class "author" ] [ text author ]
                        , div [ class "count" ] [ text <| fromInt count ]
                        ]
                    ]
                )
            )
            (sortBooks sort reverseSort books)
        )
