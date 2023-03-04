module Views.AuthorInfo exposing (authorInfo)

import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (class)
import List exposing (foldl, length)
import Model exposing (Book)
import Msg exposing (Msg)
import Utils exposing (excerptCountLabel, titleCountLabel)


authorInfo : String -> List Book -> Html Msg
authorInfo author books =
    div
        [ class "authorInfo" ]
        [ h1 [] [ text author ]
        , h2
            []
            [ titleCountLabel
                (length books)
                ++ ", "
                ++ (books
                        |> foldl
                            (\{ count } acc ->
                                acc + count
                            )
                            0
                        |> excerptCountLabel
                   )
                |> text
            ]
        ]
