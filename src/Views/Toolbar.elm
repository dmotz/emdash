module Views.Toolbar exposing (toolbar)

import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (alt, class, href, src)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))


toolbar : Html Msg
toolbar =
    div
        [ class "toolbar" ]
        [ a
            [ href "/settings" ]
            [ img
                [ class "icon"
                , src "/images/icons/settings.svg"
                , alt "Settings et cetera"
                ]
                []
            , div [ class "hint left" ] [ text "Settings &c." ]
            ]
        , button
            [ onClick ShowRandom ]
            [ img
                [ class "icon"
                , src "/images/icons/random.svg"
                , alt "Random excerpt"
                ]
                []
            , div
                [ class "hint left" ]
                [ text "Discover a random excerpt" ]
            ]
        , a
            [ href "/import" ]
            [ img
                [ class "icon"
                , src "/images/icons/import.svg"
                , alt "Import & export excerpts"
                ]
                []
            , div
                [ class "hint left" ]
                [ text "Import & export excerpts" ]
            ]
        , a
            [ href "/create" ]
            [ img
                [ class "icon"
                , src "/images/icons/create.svg"
                , alt "Create a new excerpt"
                ]
                []
            , div
                [ class "hint left" ]
                [ text "Create a new excerpt" ]
            ]
        , button
            [ onClick ScrollToTop ]
            [ img
                [ class "icon"
                , src "/images/icons/scroll-top.svg"
                , alt "Create excerpt"
                ]
                []
            , div
                [ class "hint left" ]
                [ text "Scroll to top" ]
            ]
        ]
