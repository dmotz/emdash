module Views.Create exposing (createView)

import Html
    exposing
        ( Html
        , a
        , aside
        , button
        , datalist
        , div
        , em
        , form
        , h1
        , input
        , label
        , option
        , span
        , text
        , textarea
        )
import Html.Attributes as H
    exposing
        ( class
        , disabled
        , href
        , id
        , list
        , placeholder
        , spellcheck
        , type_
        , value
        )
import Html.Events exposing (onBlur, onClick, onInput)
import List exposing (map)
import Model exposing (Author, PendingExcerpt, Title)
import Msg exposing (Msg(..))
import String exposing (isEmpty)


createView : PendingExcerpt -> List Title -> List Author -> Html Msg
createView pendingExcerpt titles authors =
    div
        [ class "createPage" ]
        [ h1 [] [ text "Create a new excerpt" ]
        , aside []
            [ text "To create excerpts in bulk, visit the "
            , a [ href "/import" ] [ text "import page" ]
            , text "."
            ]
        , form
            []
            [ label
                []
                [ textarea
                    [ value pendingExcerpt.text
                    , onInput
                        (\s ->
                            UpdatePendingExcerpt
                                { pendingExcerpt | text = s }
                        )
                    , spellcheck False
                    , placeholder "Paste excerpt text here"
                    ]
                    []
                , text "Excerpt text"
                ]
            , let
                listId =
                    "titleList"
              in
              label
                []
                [ input
                    [ value pendingExcerpt.title
                    , list listId
                    , onInput
                        (\s ->
                            UpdatePendingExcerpt
                                { pendingExcerpt | title = s }
                        )
                    , onBlur PendingTitleBlur
                    , spellcheck False
                    ]
                    []
                , text "Book title"
                , datalist
                    [ id listId ]
                    (map (\t -> option [ value t ] []) titles)
                ]
            , let
                listId =
                    "authorList"
              in
              label
                []
                [ input
                    [ value pendingExcerpt.author
                    , list listId
                    , onInput
                        (\s ->
                            UpdatePendingExcerpt
                                { pendingExcerpt | author = s }
                        )
                    , spellcheck False
                    ]
                    []
                , text "Author name"
                , datalist
                    [ id listId ]
                    (map (\t -> option [ value t ] []) authors)
                ]
            , div []
                [ label
                    []
                    [ input
                        [ type_ "number"
                        , H.min "0"
                        , value <|
                            case pendingExcerpt.page of
                                Just page ->
                                    if page > -1 then
                                        String.fromInt page

                                    else
                                        ""

                                _ ->
                                    ""
                        , onInput
                            (\s ->
                                UpdatePendingExcerpt
                                    { pendingExcerpt
                                        | page = String.toInt s
                                    }
                            )
                        ]
                        []
                    , div
                        []
                        [ text "Page № "
                        , em [] [ text "optional" ]
                        ]
                    ]
                , label
                    []
                    [ input [ type_ "url", value pendingExcerpt.sourceUrl ] []
                    , div
                        []
                        [ text "Source "
                        , span [ class "smallCaps" ] [ text "url" ]
                        , em [] [ text "optional" ]
                        ]
                    ]
                ]
            ]
        , button
            [ class "button"
            , onClick <| GetTime (CreateExcerpt pendingExcerpt)
            , disabled <|
                isEmpty pendingExcerpt.text
                    || isEmpty pendingExcerpt.title
                    || isEmpty pendingExcerpt.author
            ]
            [ text "Create" ]
        ]
