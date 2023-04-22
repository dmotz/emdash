module Views.MonkSignup exposing (monkSignup)

import Html
    exposing
        ( Html
        , aside
        , div
        , em
        , form
        , h2
        , img
        , input
        , li
        , section
        , text
        , ul
        )
import Html.Attributes exposing (class, placeholder, src, type_)
import Html.Events exposing (onInput, onSubmit)
import Msg exposing (Msg(..))
import Views.Button exposing (actionButton)


monkSignup : Bool -> Html Msg
monkSignup didSubmitEmail =
    section
        [ class "monk" ]
        [ img [ src "/images/landing/mushroom.png", class "mushrooms" ] []
        , div []
            [ aside [] [ text "Coming eventually" ]
            , h2 [] [ text "Monk-Mode" ]
            , ul
                []
                [ li []
                    [ text "Lenses — "
                    , em [] [ text "summarize & rephrase complex ideas" ]
                    ]
                , li
                    []
                    [ text "Socratic switch — "
                    , em [] [ text "interview your books" ]
                    ]
                , li [] [ text "Cross-device syncing and backup" ]
                , li [] [ text "Publishing / sharing excerpts" ]
                , li [] [ text "Sturdier gardening tools" ]
                ]
            , form
                [ onSubmit SubscribeToMailingList ]
                (if didSubmitEmail then
                    [ aside
                        []
                        [ text <|
                            "Thanks for joining the waitlist, "
                                ++ "weʼll be in touch with updates."
                        ]
                    ]

                 else
                    [ aside
                        []
                        [ text "Care to sign up for the waitlist?" ]
                    , div []
                        [ input
                            [ type_ "email"
                            , placeholder "Your email address"
                            , onInput UpdateMailingListEmail
                            ]
                            []
                        , actionButton [] [ text "Submit" ]
                        ]
                    ]
                )
            ]
        ]
