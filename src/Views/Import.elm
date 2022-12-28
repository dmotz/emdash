module Views.Import exposing (importView)

import Html
    exposing
        ( Html
        , button
        , code
        , details
        , div
        , em
        , h1
        , li
        , ol
        , p
        , section
        , span
        , summary
        , text
        )
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Utils exposing (null)


importView : Bool -> Html Msg
importView emptyOrDemo =
    div
        [ class "import" ]
        [ h1 [] [ text "Imports ", em [] [ text "&" ], text " exports" ]
        , section
            []
            [ div
                [ class "buttonStack" ]
                [ if emptyOrDemo then
                    p [] [ text "Ready to import your collection?" ]

                  else
                    null
                , button
                    [ class "button"
                    , onClick PickKindleFile
                    ]
                    [ text "Import from Kindle "
                    ]
                , p []
                    [ text "Import new excerpts from a Kindle clippings file."
                    , details
                        []
                        [ summary [] [ text "How?" ]
                        , ol []
                            [ li
                                []
                                [ text "Plug your Kindle in via USB" ]
                            , li
                                []
                                [ text "Find "
                                , code
                                    []
                                    [ text "Kindle/Documents/My Clippings.txt" ]
                                , text " in a file browser"
                                ]
                            , li
                                []
                                [ text "Drag it onto this page or click the button above" ]
                            , li
                                []
                                [ text "Repeat this process whenever you highlight new excerpts and theyʼll be added to your collection." ]
                            ]
                        ]
                    ]
                , button
                    [ class "button"
                    , onClick ImportJson
                    ]
                    [ text "Import "
                    , span
                        [ class "smallCaps" ]
                        [ text "json" ]
                    ]
                , p []
                    [ text "Restore your collection and all metadata via a previously exported Marginalia "
                    , span [ class "smallCaps" ] [ text "json" ]
                    , text " file. This will replace all existing state."
                    ]
                ]
            , div
                [ class "buttonStack" ]
                [ button
                    [ class "button"
                    , onClick ExportJson
                    ]
                    [ text "Export "
                    , span
                        [ class "smallCaps" ]
                        [ text "json" ]
                    ]
                , p
                    []
                    [ text "Exports your full collection including tags, notes, and ratings for safekeeping." ]
                , button
                    [ class "button"
                    , onClick ExportEpub
                    ]
                    [ text "Export "
                    , span
                        [ class "smallCaps" ]
                        [ text "epub" ]
                    ]
                , p []
                    [ text "Exports your excerpts into an organized "
                    , span [ class "smallCaps" ] [ text "epub" ]
                    , text " file for review on an e-reader."
                    ]
                ]
            ]
        ]
