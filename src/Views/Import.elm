module Views.Import exposing (importView)

import File
import Html
    exposing
        ( Attribute
        , Html
        , aside
        , br
        , button
        , code
        , details
        , div
        , em
        , h1
        , h3
        , li
        , ol
        , p
        , section
        , span
        , summary
        , text
        )
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)
import Msg exposing (Msg(..))
import Utils exposing (appName, null)


importView : Bool -> Bool -> Html Msg
importView emptyOrDemo isDragging =
    div
        [ class "import" ]
        [ h1 [] [ text "Imports ", em [] [ text "&" ], text " exports" ]
        , section
            []
            [ if emptyOrDemo then
                aside
                    []
                    [ text "Ready to import your collection? Drop a file or read the instructions below." ]

              else
                null
            , div
                [ classList [ ( "dropZone", True ), ( "active", isDragging ) ]
                , on "dragenter" (Decode.succeed DragEnter)
                , on "dragover" (Decode.succeed DragEnter)
                , on "dragleave" (Decode.succeed DragLeave)
                , on "drop" dropDecoder
                ]
                [ p
                    []
                    [ h3 [] [ text "Drop a file here" ]
                    , text "(Kindle clippings "
                    , span [ class "smallCaps" ] [ text "txt" ]
                    , text ", "
                    , span [ class "smallCaps" ] [ text "csv" ]
                    , text ", or "
                    , span [ class "smallCaps" ] [ text "json" ]
                    , text ")"
                    ]
                ]
            , div []
                [ div
                    [ class "buttonStack" ]
                    [ button
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
                                    [ text "Plug your Kindle in via "
                                    , span [ class "smallCaps" ] [ text "usb" ]
                                    , text "."
                                    ]
                                , li
                                    []
                                    [ text "Find "
                                    , code
                                        []
                                        [ text "Kindle/Documents/My Clippings.txt" ]
                                    , text " in a file browser."
                                    ]
                                , li
                                    []
                                    [ text "Drag it onto this page or click the button above." ]
                                , li
                                    []
                                    [ text "Repeat this process whenever you highlight new excerpts and theyʼll be added to your collection." ]
                                ]
                            ]
                        ]
                    , button
                        [ class "button"
                        , onClick ImportCsv
                        ]
                        [ text "Import "
                        , span
                            [ class "smallCaps" ]
                            [ text "csv" ]
                        ]
                    , p []
                        [ text "Import new excerpts from a "
                        , span [ class "smallCaps" ] [ text "csv" ]
                        , text " file."
                        , details
                            []
                            [ summary [] [ text "Format details" ]
                            , p []
                                [ text "Provide rows of excerpts in the following schema: "
                                , br [] []
                                , code
                                    []
                                    [ text "title, author, text, pageNum (optional), date (unix time, optional), notes (optional)" ]
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
                        [ text <|
                            "Restore your collection and all settings via a previously exported "
                                ++ appName
                                ++ " "
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
                        , onClick (GetTime ExportEpub)
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
        ]


dropDecoder : Decoder Msg
dropDecoder =
    Decode.at
        [ "dataTransfer", "files" ]
        (Decode.oneOrMore (\f _ -> GotDroppedFile f) File.decoder)


on : String -> Decoder msg -> Attribute msg
on event decoder =
    preventDefaultOn event (Decode.map (\m -> ( m, True )) decoder)
