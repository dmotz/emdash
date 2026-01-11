module Views.Landing exposing (landingPageBooks, landingView)

import Html
    exposing
        ( Html
        , a
        , aside
        , div
        , em
        , h1
        , h2
        , h3
        , hr
        , img
        , li
        , main_
        , p
        , section
        , span
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( alt
        , attribute
        , class
        , draggable
        , href
        , src
        , style
        , target
        )
import Html.Events exposing (onClick)
import List exposing (drop, isEmpty, length, map, range, reverse, take)
import Msg exposing (Msg(..))
import Types exposing (Book, CountMap)
import Utils exposing (appName, getCount)
import Views.BookList exposing (bookView)
import Views.Button exposing (actionButton)
import Views.MonkSignup exposing (monkSignup)


landingView : List Book -> CountMap -> Bool -> Html Msg
landingView bookList countMap didSubmitEmail =
    let
        bookCols =
            6

        speed =
            10

        colSize =
            length bookList // bookCols

        bookLists =
            range 0 (bookCols - 1)
                |> map (\n -> bookList |> drop (n * colSize) |> take colSize)

        list1 =
            take (bookCols // 2) bookLists

        list2 =
            drop (bookCols // 2) bookLists
    in
    div
        [ class "landing" ]
        [ div
            [ class "anim", attribute "aria-hidden" "true" ]
            (if isEmpty bookList then
                []

             else
                map
                    (\col ->
                        div
                            [ class "bookShelf" ]
                            (map
                                (\books ->
                                    let
                                        bookViews =
                                            map
                                                (\book ->
                                                    bookView
                                                        book
                                                        (getCount countMap book.id)
                                                        0
                                                        False
                                                        False
                                                        True
                                                )
                                                books

                                        duration =
                                            style
                                                "animation-duration"
                                                (String.fromInt (length books * speed) ++ "s")
                                    in
                                    div
                                        [ class "bookCol" ]
                                        [ div [ duration ] bookViews
                                        , div [ duration ] bookViews
                                        ]
                                )
                                col
                            )
                    )
                    [ map reverse list2 ++ list1, list2 ++ map reverse list1 ]
            )
        , main_ []
            [ img
                [ src "/images/logo.svg", class "logo", draggable "false", alt appName ]
                []
            , section
                [ class "cta" ]
                [ h1
                    []
                    [ text <| appName ++ " uses AI to organize text snippets so you can "
                    , em [] [ text "actually remember & learn from" ]
                    , text " what you read."
                    ]
                , aside [] [ text "Oh and itʼs free & open-source." ]
                , div
                    []
                    [ actionButton
                        [ onClick StartDemo ]
                        [ text "Try an ", em [] [ text "instant" ], text " demo" ]
                    , img
                        [ src "/images/landing/flower-yellow.png"
                        , class "botanical1"
                        , alt ""
                        ]
                        []
                    , aside [] [ text "Please, click." ]
                    ]
                , img
                    [ src "/images/landing/orange.png"
                    , class "botanical2"
                    , alt ""
                    ]
                    []
                ]
            , hr [] []
            , hr [] []
            , section
                [ class "features" ]
                [ h2 [] [ text "Featuring" ]
                , div
                    []
                    [ img [ src "/images/landing/flower-white.png", alt "" ] []
                    , img [ src "/images/landing/flower-red.png", alt "" ] []
                    , ul []
                        [ li
                            []
                            [ h3 [] [ text "Conceptual cousins" ]
                            , p
                                []
                                [ text <|
                                    "On-device AI analysis finds passages with similar ideas "
                                        ++ "from other authors, often from a different angle."
                                ]
                            ]
                        , li
                            []
                            [ h3 [] [ text "Instant semantic search" ]
                            , p
                                []
                                [ text <|
                                    "Find what youʼre looking for with both full-text "
                                        ++ "search and deeper semantic matching of fuzzy ideas."
                                ]
                            ]
                        , li
                            []
                            [ h3 [] [ text "Tag, rate, note, reflect" ]
                            , p
                                []
                                [ text <|
                                    "Organize with tags, add ratings, and "
                                        ++ "annotate your thoughts. Export back to "
                                , span [ class "smallCaps" ] [ text "epub" ]
                                , text " for review on your e-reader."
                                ]
                            ]
                        , li
                            []
                            [ h3 [] [ text "Roll the dice, change your lens" ]
                            , p []
                                [ text <|
                                    "Unearth ideas youʼve forgotten about via "
                                        ++ "random discovery. Rephrase dense concepts and re-explain with metaphors."
                                ]
                            ]
                        , li
                            []
                            [ h3 [] [ text "No lock-in" ]
                            , p []
                                [ text "Bring in your highlights from your Kindle or as "
                                , span [ class "smallCaps" ] [ text "json" ]
                                , text ", "
                                , span [ class "smallCaps" ] [ text "csv" ]
                                , text ", or manual input. Export instantly to the same open formats."
                                ]
                            ]
                        , li
                            []
                            [ h3
                                []
                                [ text "Open-source "
                                , span [] [ text "&" ]
                                , text " offline first"
                                ]
                            , p
                                []
                                [ text <|
                                    "On-device analysis means your collection stays on "
                                        ++ "your device until you opt into advanced features."
                                ]
                            ]
                        ]
                    ]
                ]
            , hr [] []
            , hr [] []
            , monkSignup didSubmitEmail
            , section
                [ class "coda" ]
                [ aside [] [ text "Thank you for reading." ]
                , p
                    []
                    [ em [] [ text "ex libris " ]
                    , a
                        [ href "https://oxism.com", target "_blank" ]
                        [ text "oxism.com" ]
                    , text " • A.D. MMXXIV"
                    ]
                ]
            ]
        ]


landingPageBooks : List ( String, String )
landingPageBooks =
    [ ( "Dune", "Frank Herbert" )
    , ( "Mindfulness in Plain English", "Henepola Gunaratana" )
    , ( "The Odyssey", "Homer" )
    , ( "A Confederacy of Dunces", "John Kennedy Toole" )
    , ( "On the Shortness of Life", "Seneca the Younger" )
    , ( "How to Change Your Mind", "Michael Pollan" )
    , ( "Fragments", "Heraclitus" )
    , ( "The Enchiridion", "Epictetus" )
    , ( "The Sirens of Titan", "Kurt Vonnegut" )
    , ( "Gödel, Escher, Bach", "Douglas Hofstadter" )
    , ( "Being Aware of Being Aware", "Rupert Spira" )
    , ( "Essays and Aphorisms", "Arthur Schopenhauer" )
    , ( "Perfume", "Patrick Süskind" )
    , ( "A Brief History of Thought", "Luc Ferry" )
    , ( "Blood Meridian", "Cormac McCarthy" )
    , ( "2001", "Arthur C. Clarke" )
    , ( "Phaedo", "Plato" )
    , ( "Prometheus Rising", "Robert Anton Wilson" )
    , ( "Letter from a Birmingham Jail", "Martin Luther King Jr." )
    , ( "The Old Man and the Sea", "Ernest Hemingway" )
    , ( "Amusing Ourselves to Death", "Neil Postman" )
    , ( "The Little Prince", "Antoine de Saint-Exupéry" )
    , ( "Alice’s Adventures in Wonderland", "Lewis Carroll" )
    , ( "The Order of Time", "Carlo Rovelli" )
    , ( "Invisible Cities", "Italo Calvino" )
    , ( "Hard-Boiled Wonderland and the End of the World", "Haruki Murakami" )
    , ( "The Metamorphosis", "Franz Kafka" )
    , ( "Notes from Underground", "Fyodor Dostoevsky" )
    , ( "Heaven and Hell", "Aldous Huxley" )
    , ( "The Society of the Spectacle", "Guy Debord" )
    , ( "The Crying of Lot 49", "Thomas Pynchon" )
    , ( "Oedipus Rex", "Sophocles" )
    , ( "Civilization and its Discontents", "Sigmund Freud" )
    , ( "Ways of Seeing", "John Berger" )
    , ( "The True Believer", "Eric Hoffer" )
    , ( "Flatland", "Edwin Abbott Abbott" )
    , ( "The Iliad", "Homer" )
    , ( "The Republic", "Plato" )
    , ( "On the Genealogy of Morals", "Friedrich Nietzsche" )
    , ( "Middlemarch", "George Eliot" )
    , ( "Maxims", "François de La Rochefoucauld" )
    , ( "Simulacra and Simulation", "Jean Baudrillard" )
    , ( "Understanding Media", "Marshall McLuhan" )
    , ( "The Secret History", "Donna Tartt" )
    , ( "Pedro Páramo", "Juan Rulfo" )
    , ( "Six Easy Pieces", "Richard Feynman" )
    , ( "Seeing Like a State", "James C. Scott" )
    , ( "The Count of Monte Cristo", "Alexandre Dumas" )
    , ( "Candide", "Voltaire" )
    ]
