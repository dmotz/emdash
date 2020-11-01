module Views.Viewer exposing (viewer, viewerId)

import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , article
        , blockquote
        , div
        , em
        , h3
        , h5
        , p
        , section
        , span
        , text
        , textarea
        )
import Html.Attributes exposing (class, id, value)
import Html.Events exposing (onClick, onFocus, targetValue)
import Json.Decode as Decode
import List exposing (foldr, head, length, map)
import Maybe exposing (withDefault)
import Model exposing (Entry, Filter(..), Id, InputFocus(..), Tag)
import Msg exposing (Msg(..))
import Set
import String exposing (fromInt)
import Views.Common exposing (on)
import Views.Intro exposing (intro)
import Views.Neighbors exposing (neighbors)
import Views.TagSection exposing (tagSection)


viewer :
    List Entry
    -> Bool
    -> Bool
    -> List Tag
    -> Maybe Tag
    -> Dict Id (List ( Entry, Float ))
    -> ( Int, Int )
    -> Html Msg
viewer selectedEntries parsingError noEntries tags pendingTag neighborMap ( completed, total ) =
    article
        (id viewerId
            :: (if parsingError then
                    [ onClick ResetError ]

                else
                    []
               )
        )
        [ case selectedEntries of
            [ entry ] ->
                div []
                    [ blockquote [] [ text entry.text ]
                    , Html.cite [ id "meta" ]
                        ([ span
                            [ class "title"
                            , onClick <| FilterBy TitleFilter entry.title
                            ]
                            [ text entry.title ]
                         , span [ class "sep" ] [ text "•" ]
                         , span
                            [ class "author"
                            , onClick <| FilterBy AuthorFilter entry.author
                            ]
                            [ text entry.author ]
                         ]
                            ++ (case entry.page of
                                    Just n ->
                                        [ span [ class "sep" ] [ text "•" ]
                                        , span
                                            [ class "page" ]
                                            [ text <| "p. " ++ fromInt n ]
                                        ]

                                    _ ->
                                        []
                               )
                        )
                    , neighbors entry neighborMap completed total
                    , section
                        [ id "entry-tools" ]
                        [ tagSection entry.tags tags pendingTag
                        , section []
                            [ h5 [] [ text "notes:" ]
                            , textarea
                                [ onFocus <| SetInputFocus (Just NoteFocus)
                                , onBlurVal UpdateNotes
                                , value entry.notes
                                ]
                                [ text entry.notes ]
                            ]
                        , hideButton [ entry ]
                        ]
                    ]

            [] ->
                div [ id "intro", class "info-page" ]
                    [ if parsingError then
                        p [ class "error" ] [ text "Error parsing file." ]

                      else if noEntries then
                        intro

                      else
                        text ""
                    ]

            entries ->
                let
                    titleCount =
                        entries
                            |> map .title
                            |> Set.fromList
                            |> Set.size
                in
                div []
                    [ h3 []
                        [ text <|
                            (entries |> length |> fromInt)
                                ++ " entries from "
                        , if titleCount > 1 then
                            text <| fromInt titleCount ++ " titles"

                          else
                            case entries of
                                entry :: _ ->
                                    em [] [ text entry.title ]

                                _ ->
                                    text ""
                        ]
                    , div [ id "entry-tools" ]
                        [ tagSection
                            (foldr
                                (\entry set ->
                                    Set.intersect set (Set.fromList entry.tags)
                                )
                                (withDefault
                                    Set.empty
                                    (entries
                                        |> head
                                        |> Maybe.map (.tags >> Set.fromList)
                                    )
                                )
                                entries
                                |> Set.toList
                            )
                            tags
                            pendingTag
                        , hideButton entries
                        ]
                    ]
        ]


hideButton : List Entry -> Html Msg
hideButton entries =
    section []
        [ div
            [ class "hide-button", onClick <| PromptHide ]
            [ div [] [ text "×" ]
            , span []
                [ text <|
                    "delete entr"
                        ++ (case entries of
                                [ _ ] ->
                                    "y"

                                _ ->
                                    "ies"
                           )
                ]
            ]
        ]


viewerId : String
viewerId =
    "viewer"


onBlurVal : (String -> msg) -> Attribute msg
onBlurVal ev =
    on "blur" (Decode.map ev targetValue)
