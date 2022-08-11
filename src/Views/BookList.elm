module Views.BookList exposing (bookList)

import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (class, href)
import List exposing (map)
import Model exposing (Book)
import Msg exposing (Msg)
import Router exposing (titleToRoute)
import String exposing (fromInt)


bookList : List Book -> Html Msg
bookList books =
    ul
        [ class "books" ]
        (map
            (\{ title, author, count } ->
                li []
                    [ a
                        [ href <| titleToRoute title ]
                        [ div [ class "title" ] [ text title ]
                        , div [ class "author" ] [ text author ]
                        , div [ class "count" ] [ text <| fromInt count ]
                        ]
                    ]
            )
            books
        )
