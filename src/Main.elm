port module Main exposing (main)

import Browser exposing (application)
import Browser.Dom exposing (getElement, setViewport)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Debounce
import Decoder exposing (decodeStoredModel)
import Dict exposing (get, insert, keys, remove, values)
import Epub
import File
import File.Select as Select
import Http
import Json.Decode as Decode
import List
    exposing
        ( concatMap
        , drop
        , filter
        , filterMap
        , foldl
        , head
        , isEmpty
        , length
        , map
        , member
        , sort
        , sortBy
        , take
        )
import Maybe exposing (andThen, withDefault)
import Model
    exposing
        ( BookSort(..)
        , ExcerptSort(..)
        , Id
        , InputFocus(..)
        , Model
        , Page(..)
        , PendingExcerpt
        , ScorePairs
        , SearchMode(..)
        , StoredModel
        , TagSort(..)
        , initialStoredModel
        )
import Msg exposing (Msg(..))
import Parser exposing (getExcerptId)
import Platform.Cmd exposing (batch, none)
import Random exposing (generate)
import Router
    exposing
        ( Route(..)
        , excerptToRoute
        , routeParser
        , searchToRoute
        , slugify
        )
import Set exposing (diff, toList, union)
import String exposing (fromInt, join, toLower, trim)
import Task exposing (attempt, perform)
import Time exposing (posixToMillis)
import Update.Extra as Update exposing (addCmd)
import Url exposing (Url)
import Url.Parser exposing (parse)
import Utils
    exposing
        ( KeyEvent
        , dedupe
        , findMatches
        , getTagCounts
        , insertOnce
        , juxt
        , modelToStoredModel
        , removeItem
        , untaggedKey
        )
import Views.Base exposing (view)


port setStorage : StoredModel -> Cmd msg


port scrollToTop : () -> Cmd msg


port exportJson : StoredModel -> Cmd msg


port handleNewExcerpts : StoredModel -> Cmd msg


port requestExcerptEmbeddings : List ( Id, String ) -> Cmd msg


port receiveExcerptEmbeddings : (List Id -> msg) -> Sub msg


port requestBookEmbeddings : List ( Id, List Id ) -> Cmd msg


port receiveBookEmbeddings : (() -> msg) -> Sub msg


port deleteEmbedding : Id -> Cmd msg


port requestExcerptNeighbors : ( Id, Bool ) -> Cmd msg


port requestBookNeighbors : Id -> Cmd msg


port receiveExcerptNeighbors : (( Id, ScorePairs ) -> msg) -> Sub msg


port receiveBookNeighbors : (( Id, ScorePairs ) -> msg) -> Sub msg


port requestSemanticRank : ( Id, List Id ) -> Cmd msg


port receiveSemanticRank : (( Id, ScorePairs ) -> msg) -> Sub msg


port requestUnicodeNormalized : String -> Cmd msg


port receiveUnicodeNormalized : (String -> msg) -> Sub msg


port requestSemanticSearch : ( String, Float ) -> Cmd msg


port receiveSemanticSearch : (( String, ScorePairs ) -> msg) -> Sub msg


port fetchDemoEmbeddings : List Id -> Cmd msg


appName : String
appName =
    "Marginalia"


minSemanticQueryLen : Int
minSemanticQueryLen =
    5


embeddingBatchSize : Int
embeddingBatchSize =
    10


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.soon 999
    , transform = DebounceMsg
    }


createModel : String -> Maybe StoredModel -> Bool -> Url -> Nav.Key -> Model
createModel version mStoredModel demoMode url key =
    let
        restored =
            withDefault initialStoredModel mStoredModel

        ( titleRouteMap, booksWithSlugs ) =
            Parser.getTitleRouteMap restored.books

        books =
            Dict.fromList (map (juxt .id identity) booksWithSlugs)

        tags =
            restored.books |> concatMap .tags |> dedupe
    in
    { page = MainPage (values books) Nothing
    , demoMode = demoMode
    , excerpts = Dict.fromList (map (juxt .id identity) restored.excerpts)
    , books = books
    , semanticThreshold = 0.1
    , neighborMap = Dict.empty
    , bookNeighborMap = Dict.empty
    , semanticRankMap = Dict.empty
    , hiddenExcerpts = Set.fromList restored.hiddenExcerpts
    , completedEmbeddings = Set.empty
    , embeddingsReady = False
    , tags = restored.books |> concatMap .tags |> dedupe
    , tagCounts = getTagCounts books
    , tagSort = TagAlphaSort
    , showTagHeader = length tags > 0
    , titleRouteMap = titleRouteMap
    , authorRouteMap = Parser.getAuthorRouteMap restored.books
    , pendingTag = Nothing
    , isDragging = False
    , reverseSort = True
    , inputFocused = Nothing
    , parsingError = Nothing
    , url = url
    , key = key
    , bookSort = RecencySort
    , excerptSort = ExcerptPageSort
    , bookmarks = restored.bookmarks |> Dict.fromList
    , idToShowDetails = Dict.empty
    , idToActiveTab = Dict.empty
    , searchQuery = ""
    , searchDebounce = Debounce.init
    , version = version
    }


main : Program ( String, Maybe String ) Model Msg
main =
    application
        { init = init
        , update = update
        , view =
            \m ->
                { title =
                    case m.page of
                        MainPage _ Nothing ->
                            appName

                        LandingPage ->
                            appName

                        _ ->
                            (case m.page of
                                MainPage _ (Just tag) ->
                                    "#" ++ tag

                                SearchPage query _ _ _ _ ->
                                    "🔍 " ++ query

                                TitlePage book _ ->
                                    book.title

                                AuthorPage author _ ->
                                    author

                                ExcerptPage excerpt book ->
                                    book.title
                                        ++ " "
                                        ++ "p. "
                                        ++ fromInt excerpt.page

                                SettingsPage ->
                                    "Settings"

                                ImportPage ->
                                    "Import"

                                CreatePage _ _ _ ->
                                    "New excerpt"

                                NotFoundPage _ ->
                                    "404"

                                _ ->
                                    ""
                            )
                                ++ " | "
                                ++ appName
                , body = [ view m ]
                }
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Decode.map3 KeyEvent
                        (Decode.field "key" Decode.string)
                        (Decode.field "ctrlKey" Decode.bool)
                        (Decode.field "metaKey" Decode.bool)
                        |> Decode.map KeyDown
                        |> onKeyDown
                    , receiveExcerptNeighbors ReceiveNeighbors
                    , receiveBookNeighbors ReceiveBookNeighbors
                    , receiveExcerptEmbeddings ReceiveEmbeddings
                    , receiveBookEmbeddings ReceiveBookEmbeddings
                    , receiveUnicodeNormalized ReceiveUnicodeNormalized
                    , receiveSemanticSearch ReceiveSemanticSearch
                    , receiveSemanticRank ReceiveSemanticRank
                    ]
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : ( String, Maybe String ) -> Url -> Nav.Key -> ( Model, Cmd Msg )
init ( version, mStateString ) url key =
    let
        model =
            createModel
                version
                (case decodeStoredModel (withDefault "" mStateString) of
                    Ok storedModel ->
                        Just storedModel

                    _ ->
                        Nothing
                )
                False
                url
                key
    in
    update (UrlChanged url) model
        |> addCmd (model |> modelToStoredModel |> handleNewExcerpts)


store : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
store ( model, cmd ) =
    if model.demoMode then
        ( model, cmd )

    else
        ( model, batch [ cmd, model |> modelToStoredModel |> setStorage ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        noOp =
            ( model, none )
    in
    case message of
        NoOp ->
            noOp

        RestoreState maybeModel demoMode ->
            let
                model_ =
                    createModel
                        model.version
                        maybeModel
                        demoMode
                        model.url
                        model.key
            in
            update (UrlChanged model.url) model_
                |> addCmd
                    (batch
                        [ model_ |> modelToStoredModel |> handleNewExcerpts
                        , if demoMode then
                            fetchDemoEmbeddings (keys model_.excerpts)

                          else
                            none
                        ]
                    )

        ParseJsonText text ->
            case decodeStoredModel text of
                Ok storedModel ->
                    update
                        (RestoreState (Just storedModel) model.demoMode)
                        model

                Err e ->
                    ( { model | parsingError = Just (Decode.errorToString e) }
                    , none
                    )

        DragEnter ->
            ( { model | isDragging = True }, none )

        DragLeave ->
            ( { model | isDragging = False }, none )

        GotFile msg file ->
            ( { model | isDragging = False }
            , perform msg (File.toString file)
            )

        PickKindleFile ->
            ( model, Select.file [ "text/plain" ] (GotFile LoadKindleFile) )

        LoadKindleFile text ->
            ( model, requestUnicodeNormalized text )

        ReceiveUnicodeNormalized text ->
            let
                ( newExcerpts, newBooks ) =
                    Parser.process text

                hiddenPred =
                    \id _ -> not <| Set.member id model.hiddenExcerpts

                unseenExcerpts =
                    Dict.diff newExcerpts model.excerpts |> Dict.filter hiddenPred

                bookVals =
                    unseenExcerpts
                        |> Dict.foldl
                            (\_ excerpt acc ->
                                Dict.update
                                    excerpt.bookId
                                    (Maybe.map
                                        (\book ->
                                            { book
                                                | count = book.count + 1
                                                , sortIndex =
                                                    max
                                                        book.sortIndex
                                                        excerpt.date
                                            }
                                        )
                                    )
                                    acc
                            )
                            (Dict.union model.books newBooks)
                        |> values

                ( titleRouteMap, booksWithSlugs ) =
                    Parser.getTitleRouteMap bookVals
            in
            if Dict.isEmpty newExcerpts then
                ( { model | parsingError = Just "Failed to parse file." }, none )

            else
                ( { model
                    | parsingError = Nothing
                    , excerpts =
                        Dict.union model.excerpts newExcerpts
                            |> Dict.filter hiddenPred
                    , books =
                        Dict.fromList
                            (map (juxt .id identity) booksWithSlugs)
                    , titleRouteMap = titleRouteMap
                    , authorRouteMap =
                        Parser.getAuthorRouteMap bookVals
                    , embeddingsReady = False
                    , neighborMap = Dict.empty
                  }
                , none
                )
                    |> Update.andThen update (SortBooks model.bookSort)
                    |> Update.andThen update (UrlChanged model.url)
                    |> addCmd (model |> modelToStoredModel |> handleNewExcerpts)
                    |> store

        ResetError ->
            ( { model | parsingError = Nothing }, none )

        ShowRandom ->
            ( model
            , generate
                GotRandomIndex
                (Random.int 0 ((model.excerpts |> values |> length) - 1))
            )

        GotRandomIndex n ->
            case model.excerpts |> values |> drop n |> head of
                Just excerpt ->
                    ( model
                    , Nav.pushUrl model.key (excerptToRoute model.books excerpt)
                    )

                _ ->
                    noOp

        SetInputFocus focus ->
            ( { model | inputFocused = focus }, none )

        UpdateNotes id text ->
            store
                ( { model
                    | excerpts =
                        Dict.update
                            id
                            (Maybe.map (\excerpt -> { excerpt | notes = text }))
                            model.excerpts
                    , page =
                        case model.page of
                            ExcerptPage excerpt book ->
                                ExcerptPage { excerpt | notes = text } book

                            TitlePage book excerpts ->
                                TitlePage
                                    book
                                    (map
                                        (\excerpt ->
                                            if excerpt.id == id then
                                                { excerpt | notes = text }

                                            else
                                                excerpt
                                        )
                                        excerpts
                                    )

                            _ ->
                                model.page
                  }
                , none
                )

        UpdatePendingTag text ->
            ( { model | pendingTag = Just text }, none )

        AddTag ->
            case model.page of
                TitlePage book excerpts ->
                    case model.pendingTag of
                        Just tag ->
                            let
                                tagN =
                                    tag |> trim |> toLower |> slugify

                                newTagSet =
                                    insertOnce book.tags tagN
                            in
                            if tagN == "" || tagN == untaggedKey then
                                ( { model | pendingTag = Nothing }, none )

                            else
                                let
                                    books =
                                        Dict.update book.id
                                            (Maybe.map
                                                (\b -> { b | tags = newTagSet })
                                            )
                                            model.books
                                in
                                store
                                    ( { model
                                        | books = books
                                        , tags = insertOnce model.tags tagN
                                        , tagCounts = getTagCounts books
                                        , pendingTag = Nothing
                                        , page =
                                            TitlePage
                                                { book | tags = newTagSet }
                                                excerpts
                                      }
                                    , none
                                    )

                        _ ->
                            noOp

                _ ->
                    noOp

        RemoveTag tag ->
            case model.page of
                TitlePage book excerpts ->
                    let
                        newTagSet =
                            removeItem book.tags tag

                        books =
                            Dict.update
                                book.id
                                (Maybe.map (\b -> { b | tags = newTagSet }))
                                model.books
                    in
                    store
                        ( { model
                            | books = books
                            , tags =
                                books
                                    |> values
                                    |> concatMap .tags
                                    |> dedupe
                            , tagCounts = getTagCounts books
                            , page =
                                TitlePage
                                    { book | tags = newTagSet }
                                    excerpts
                          }
                        , none
                        )

                _ ->
                    noOp

        SetRating book n ->
            let
                newBook =
                    { book | rating = n }
            in
            store
                ( { model
                    | books = insert book.id newBook model.books
                    , page =
                        case model.page of
                            TitlePage _ excerpts ->
                                TitlePage newBook excerpts

                            _ ->
                                model.page
                  }
                , none
                )

        SetTagSort sort ->
            ( { model | tagSort = sort }, none )

        HideExcerpt excerpt ->
            let
                excerpts =
                    remove excerpt.id model.excerpts

                ( books, bookmarks ) =
                    case get excerpt.bookId model.books of
                        Just book ->
                            if book.count == 1 then
                                ( remove book.id model.books
                                , remove book.id model.bookmarks
                                )

                            else
                                ( Dict.update
                                    book.id
                                    (Maybe.map
                                        (\b ->
                                            { b
                                                | count = b.count - 1
                                                , favCount =
                                                    if excerpt.isFavorite then
                                                        b.favCount - 1

                                                    else
                                                        b.favCount
                                            }
                                        )
                                    )
                                    model.books
                                , case get book.id model.bookmarks of
                                    Just id ->
                                        if id == excerpt.id then
                                            remove book.id model.bookmarks

                                        else
                                            model.bookmarks

                                    _ ->
                                        model.bookmarks
                                )

                        _ ->
                            ( model.books, model.bookmarks )
            in
            store
                ( { model
                    | hiddenExcerpts = Set.insert excerpt.id model.hiddenExcerpts
                    , excerpts = excerpts
                    , books = books
                    , bookmarks = bookmarks
                    , page =
                        case model.page of
                            TitlePage oldBook oldExcerpts ->
                                TitlePage
                                    (withDefault oldBook (get oldBook.id books))
                                    (filter
                                        (\e -> e.id /= excerpt.id)
                                        oldExcerpts
                                    )

                            _ ->
                                model.page
                    , tagCounts = getTagCounts books
                    , completedEmbeddings =
                        Set.remove excerpt.id model.completedEmbeddings
                    , neighborMap = Dict.empty
                    , bookNeighborMap = Dict.empty
                  }
                , batch
                    [ deleteEmbedding excerpt.id
                    , case model.page of
                        ExcerptPage { id } _ ->
                            if id == excerpt.id then
                                Nav.pushUrl model.key "/"

                            else
                                none

                        TitlePage book ents ->
                            if book.id == excerpt.bookId && length ents == 1 then
                                Nav.pushUrl model.key "/"

                            else
                                none

                        _ ->
                            none
                    ]
                )

        Sort ->
            store ( { model | reverseSort = not model.reverseSort }, none )

        ToggleTagHeader ->
            ( { model | showTagHeader = not model.showTagHeader }, none )

        ScrollToElement result ->
            case result of
                Ok element ->
                    ( model
                    , perform (always NoOp) (setViewport 0 element.element.y)
                    )

                Err _ ->
                    noOp

        ExportJson ->
            ( model, model |> modelToStoredModel |> exportJson )

        ImportJson ->
            ( model
            , Select.file [ "application/json" ] (GotFile ParseJsonText)
            )

        KeyDown { key, control, meta } ->
            if control || meta then
                noOp

            else if model.inputFocused /= Nothing then
                if key == "Enter" && model.inputFocused == Just TagFocus then
                    update AddTag model

                else
                    noOp

            else
                noOp

        ExportEpub time ->
            ( model, Epub.export time (values model.books) (values model.excerpts) )

        RequestEmbeddings ->
            let
                nextBatch =
                    diff
                        (diff
                            (model.excerpts |> keys |> Set.fromList)
                            model.completedEmbeddings
                        )
                        model.hiddenExcerpts
                        |> toList
                        |> filterMap (\id -> get id model.excerpts)
                        |> map (\excerpt -> ( excerpt.id, excerpt.text ))
                        |> take embeddingBatchSize
            in
            if isEmpty nextBatch then
                ( model
                , model.books
                    |> values
                    |> map
                        (\{ id } ->
                            ( id
                            , model.excerpts
                                |> values
                                |> filter (.bookId >> (==) id)
                                |> map .id
                            )
                        )
                    |> requestBookEmbeddings
                )

            else
                ( { model | embeddingsReady = False }
                , requestExcerptEmbeddings nextBatch
                )

        ReceiveEmbeddings ids ->
            update
                RequestEmbeddings
                { model
                    | completedEmbeddings =
                        union model.completedEmbeddings (Set.fromList ids)
                }

        ReceiveBookEmbeddings _ ->
            ( { model | embeddingsReady = True }
            , case model.page of
                TitlePage book _ ->
                    batch
                        [ requestBookNeighbors book.id
                        , requestSemanticRank
                            ( book.id
                            , model.excerpts
                                |> Dict.filter
                                    (\_ { bookId } ->
                                        bookId == book.id
                                    )
                                |> values
                                |> map .id
                            )
                        ]

                ExcerptPage excerpt _ ->
                    requestExcerptNeighbors ( excerpt.id, True )

                SearchPage query _ _ _ _ ->
                    requestSemanticSearch ( query, model.semanticThreshold )

                _ ->
                    none
            )

        ReceiveNeighbors ( targetId, idScores ) ->
            if Dict.member targetId model.excerpts then
                ( { model
                    | neighborMap =
                        insert
                            targetId
                            (filterMap
                                (\( id, score ) ->
                                    case get id model.excerpts of
                                        Just excerpt ->
                                            Just ( excerpt.id, score )

                                        _ ->
                                            Nothing
                                )
                                idScores
                            )
                            model.neighborMap
                  }
                , none
                )

            else
                noOp

        ReceiveBookNeighbors ( targetId, idScores ) ->
            if Dict.member targetId model.books then
                ( { model
                    | bookNeighborMap =
                        insert
                            targetId
                            (filterMap
                                (\( id, score ) ->
                                    case get id model.books of
                                        Just book ->
                                            Just ( book.id, score )

                                        _ ->
                                            Nothing
                                )
                                idScores
                            )
                            model.bookNeighborMap
                  }
                , none
                )

            else
                noOp

        ReceiveSemanticSearch ( _, idScores ) ->
            case model.page of
                SearchPage query mode books excerpts _ ->
                    ( { model
                        | page =
                            SearchPage
                                query
                                mode
                                books
                                excerpts
                                (filter
                                    (\( id, _ ) ->
                                        not <|
                                            foldl
                                                (\ent acc ->
                                                    acc
                                                        || ent.id
                                                        == id
                                                )
                                                False
                                                excerpts
                                    )
                                    idScores
                                )
                      }
                    , none
                    )

                _ ->
                    noOp

        ReceiveSemanticRank ( bookId, ids ) ->
            let
                model_ =
                    { model
                        | semanticRankMap =
                            insert
                                bookId
                                ids
                                model.semanticRankMap
                    }
            in
            case model.page of
                TitlePage book _ ->
                    if book.id == bookId && model.excerptSort == ExcerptSemanticSort then
                        update (SortExcerpts model.excerptSort) model_

                    else
                        ( model_, none )

                _ ->
                    ( model_, none )

        SetSemanticThreshold s ->
            case String.toFloat s of
                Just n ->
                    ( { model | semanticThreshold = n }, none )

                _ ->
                    noOp

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if url == model.url then
                        noOp

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                model_ =
                    { model | url = url }

                scrollTop =
                    perform (always NoOp) (setViewport 0 0)
            in
            case
                parse routeParser url
            of
                Just RootRoute ->
                    ( { model_
                        | page =
                            if Dict.isEmpty model.books then
                                LandingPage

                            else
                                MainPage (values model.books) Nothing
                        , searchQuery = ""
                      }
                    , scrollTop
                    )

                Just (TitleRoute slug mFragment) ->
                    case
                        get slug model.titleRouteMap
                            |> andThen (\id -> get id model.books)
                    of
                        Just book ->
                            let
                                excerpts =
                                    model.excerpts
                                        |> Dict.filter
                                            (\_ { bookId } ->
                                                bookId == book.id
                                            )
                                        |> values
                                        |> sortBy .page
                            in
                            ( { model_
                                | page = TitlePage book excerpts
                                , excerptSort = ExcerptPageSort
                              }
                            , batch
                                ((case mFragment of
                                    Just excerptId ->
                                        attempt
                                            ScrollToElement
                                            (getElement excerptId)

                                    _ ->
                                        case parse routeParser model.url of
                                            Just (TitleRoute lastSlug _) ->
                                                if lastSlug == slug then
                                                    none

                                                else
                                                    scrollTop

                                            _ ->
                                                scrollTop
                                 )
                                    :: (if model.embeddingsReady then
                                            [ requestBookNeighbors book.id
                                            , requestSemanticRank
                                                ( book.id
                                                , map .id excerpts
                                                )
                                            ]

                                        else
                                            []
                                       )
                                )
                            )

                        _ ->
                            ( { model_
                                | page = NotFoundPage "Title not found."
                              }
                            , none
                            )

                Just (ExcerptRoute titleSlug excerptSlug) ->
                    let
                        mExcerpt =
                            get excerptSlug model.excerpts

                        mBook =
                            get titleSlug model.titleRouteMap
                                |> andThen (\id -> get id model.books)
                    in
                    case ( mExcerpt, mBook ) of
                        ( Just excerpt, Just book ) ->
                            ( { model_ | page = ExcerptPage excerpt book }
                            , batch
                                [ scrollTop
                                , if model.embeddingsReady then
                                    requestExcerptNeighbors ( excerpt.id, True )

                                  else
                                    none
                                ]
                            )

                        _ ->
                            ( { model_
                                | page = NotFoundPage "Excerpt not found."
                              }
                            , none
                            )

                Just (AuthorRoute slug) ->
                    case get slug model.authorRouteMap of
                        Just author ->
                            ( { model_
                                | page =
                                    AuthorPage author
                                        (model.books
                                            |> Dict.filter
                                                (\_ b -> member author b.authors)
                                            |> values
                                        )
                              }
                            , scrollTop
                            )

                        _ ->
                            ( { model_
                                | page = NotFoundPage "Author not found."
                              }
                            , none
                            )

                Just (TagRoute tag) ->
                    if tag == untaggedKey || member tag model.tags then
                        ( { model_
                            | page =
                                MainPage
                                    (Dict.filter
                                        (if tag == untaggedKey then
                                            \_ book -> isEmpty book.tags

                                         else
                                            \_ book -> member tag book.tags
                                        )
                                        model.books
                                        |> values
                                    )
                                    (Just tag)
                          }
                        , none
                        )

                    else
                        ( { model_
                            | page = NotFoundPage "Tag not found."
                          }
                        , none
                        )

                Just (SearchRoute query) ->
                    case query of
                        Just text ->
                            let
                                ( debounce, cmd ) =
                                    Debounce.push
                                        debounceConfig
                                        text
                                        model.searchDebounce
                            in
                            ( { model_
                                | searchDebounce = debounce
                                , searchQuery = text
                              }
                            , cmd
                            )

                        _ ->
                            ( { model_ | searchQuery = "" }, none )

                Just SettingsRoute ->
                    ( { model_ | page = SettingsPage }, scrollTop )

                Just ImportRoute ->
                    ( { model_ | page = ImportPage }, scrollTop )

                Just CreateRoute ->
                    ( { model_
                        | page =
                            CreatePage
                                (PendingExcerpt "" "" "" -1)
                                (values model.books |> map .title |> sort)
                                (values model.authorRouteMap)
                      }
                    , scrollTop
                    )

                _ ->
                    ( { model_ | page = NotFoundPage "Route not found." }
                    , none
                    )

        SortBooks sort ->
            ( { model | bookSort = sort, reverseSort = sort /= TitleSort }
            , none
            )

        SortExcerpts sort ->
            ( { model
                | excerptSort = sort
                , page =
                    case model.page of
                        TitlePage book excerpts ->
                            TitlePage
                                book
                                (case sort of
                                    ExcerptSemanticSort ->
                                        case get book.id model.semanticRankMap of
                                            Just ids ->
                                                filterMap
                                                    (\( id, _ ) ->
                                                        get id model.excerpts
                                                    )
                                                    ids

                                            _ ->
                                                sortBy .page excerpts

                                    _ ->
                                        sortBy .page excerpts
                                )

                        _ ->
                            model.page
              }
            , none
            )

        SetBookmark bookId excerptId ->
            store
                ( { model
                    | bookmarks =
                        case get bookId model.bookmarks of
                            Just prevExcerptId ->
                                if prevExcerptId == excerptId then
                                    remove bookId model.bookmarks

                                else
                                    insert bookId excerptId model.bookmarks

                            _ ->
                                insert bookId excerptId model.bookmarks
                  }
                , none
                )

        ToggleFavorite id ->
            let
                toggle =
                    \excerpt -> { excerpt | isFavorite = not excerpt.isFavorite }

                newExcerpts =
                    Dict.update
                        id
                        (Maybe.map toggle)
                        model.excerpts

                countDelta =
                    if
                        newExcerpts
                            |> get id
                            |> Maybe.map .isFavorite
                            |> withDefault False
                    then
                        1

                    else
                        -1

                updateCount =
                    \book -> { book | favCount = book.favCount + countDelta }
            in
            store
                ( { model
                    | excerpts = newExcerpts
                    , books =
                        case get id newExcerpts of
                            Just excerpt ->
                                Dict.update
                                    excerpt.bookId
                                    (Maybe.map updateCount)
                                    model.books

                            _ ->
                                model.books
                    , page =
                        case model.page of
                            ExcerptPage excerpt book ->
                                ExcerptPage (toggle excerpt) (updateCount book)

                            TitlePage book excerpts ->
                                TitlePage
                                    (updateCount book)
                                    (map
                                        (\excerpt ->
                                            if excerpt.id == id then
                                                toggle excerpt

                                            else
                                                excerpt
                                        )
                                        excerpts
                                    )

                            _ ->
                                model.page
                  }
                , none
                )

        SetExcerptTab id tab toggle ->
            let
                m =
                    { model
                        | idToActiveTab = insert id tab model.idToActiveTab
                    }
            in
            if toggle then
                let
                    newState =
                        get id model.idToShowDetails |> withDefault False |> not
                in
                ( { m
                    | idToShowDetails = insert id newState model.idToShowDetails
                  }
                , if newState && get id model.neighborMap == Nothing then
                    requestExcerptNeighbors ( id, True )

                  else
                    none
                )

            else
                ( m, none )

        ScrollToTop ->
            ( model, scrollToTop () )

        OnSearchStart query ->
            if String.isEmpty query then
                ( { model | searchQuery = "" }, Nav.replaceUrl model.key "/" )

            else
                ( model, Nav.replaceUrl model.key (searchToRoute query) )

        OnSearchEnd val ->
            let
                query =
                    trim val

                ( mode, prevSemantic ) =
                    case model.page of
                        SearchPage _ m _ _ semanticMatches ->
                            ( Just m, Just semanticMatches )

                        _ ->
                            ( Nothing, Nothing )
            in
            if String.isEmpty query then
                noOp

            else
                ( { model
                    | page =
                        SearchPage
                            query
                            (withDefault TextMatches mode)
                            (findMatches
                                query
                                (\b -> b.title ++ " " ++ join " " b.authors)
                                (values model.books)
                            )
                            (model.excerpts
                                |> values
                                |> findMatches query .text
                            )
                            (if String.length query >= minSemanticQueryLen then
                                withDefault [] prevSemantic

                             else
                                []
                            )
                  }
                , if String.length query >= minSemanticQueryLen then
                    requestSemanticSearch ( query, model.semanticThreshold )

                  else
                    none
                )

        SetSearchTab mode ->
            case model.page of
                SearchPage query _ books excerpts semanticMatches ->
                    ( { model
                        | page =
                            SearchPage query mode books excerpts semanticMatches
                      }
                    , none
                    )

                _ ->
                    noOp

        DebounceMsg msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast
                            (\t -> Task.perform OnSearchEnd (Task.succeed t))
                        )
                        msg
                        model.searchDebounce
            in
            ( { model | searchDebounce = debounce }
            , cmd
            )

        StartDemo ->
            ( { model | demoMode = True }
            , Http.get
                { url = "/demo/demo.json"
                , expect = Http.expectString GotDemoData
                }
            )

        GotDemoData result ->
            case result of
                Ok text ->
                    update (ParseJsonText text) model

                Err _ ->
                    noOp

        GetTime msg ->
            ( model, perform msg Time.now )

        UpdatePendingExcerpt pExcerpt ->
            case model.page of
                CreatePage _ titles authors ->
                    ( { model | page = CreatePage pExcerpt titles authors }, none )

                _ ->
                    noOp

        CreateExcerpt excerpt time ->
            let
                excerptId =
                    getExcerptId excerpt.text (excerpt.title ++ " " ++ excerpt.author)
            in
            case get excerptId model.excerpts of
                Just existingExcerpt ->
                    ( model
                    , Nav.pushUrl
                        model.key
                        (excerptToRoute model.books existingExcerpt)
                    )

                _ ->
                    let
                        mBook =
                            model.books
                                |> values
                                |> filter
                                    (\b ->
                                        b.title
                                            == excerpt.title
                                            && member excerpt.author b.authors
                                    )
                                |> head

                        timestamp =
                            posixToMillis time

                        fullExcerpt =
                            \bookId ->
                                { id = excerptId
                                , text = excerpt.text
                                , bookId = bookId
                                , date = timestamp
                                , page = excerpt.page
                                , notes = ""
                                , isFavorite = False
                                }

                        model_ =
                            { model
                                | neighborMap = Dict.empty
                                , bookNeighborMap = Dict.empty
                                , embeddingsReady = False
                            }
                    in
                    store
                        (case mBook of
                            Just book ->
                                let
                                    newBooks =
                                        Dict.update
                                            book.id
                                            (Maybe.map
                                                (\b ->
                                                    { b
                                                        | count = b.count + 1
                                                        , sortIndex =
                                                            max
                                                                b.sortIndex
                                                                timestamp
                                                    }
                                                )
                                            )
                                            model.books

                                    newExcerpt =
                                        fullExcerpt book.id
                                in
                                ( { model_
                                    | books = newBooks
                                    , excerpts =
                                        insert excerptId newExcerpt model.excerpts
                                    , semanticRankMap =
                                        remove
                                            book.id
                                            model.semanticRankMap
                                    , tagCounts = getTagCounts newBooks
                                  }
                                , Nav.pushUrl
                                    model.key
                                    (excerptToRoute newBooks newExcerpt)
                                )

                            _ ->
                                let
                                    bookId =
                                        getExcerptId excerpt.title excerpt.author

                                    ( titleRouteMap, booksWithSlugs ) =
                                        insert
                                            bookId
                                            { id = bookId
                                            , title = excerpt.title
                                            , authors = [ excerpt.author ]
                                            , count = 1
                                            , rating = 0
                                            , sortIndex = timestamp
                                            , tags = []
                                            , slug = ""
                                            , favCount = 0
                                            }
                                            model.books
                                            |> values
                                            |> Parser.getTitleRouteMap

                                    newBooks =
                                        Dict.fromList
                                            (map
                                                (juxt .id identity)
                                                booksWithSlugs
                                            )

                                    newExcerpt =
                                        fullExcerpt bookId
                                in
                                ( { model_
                                    | books = newBooks
                                    , excerpts =
                                        insert excerptId newExcerpt model.excerpts
                                    , titleRouteMap = titleRouteMap
                                    , authorRouteMap =
                                        Parser.getAuthorRouteMap booksWithSlugs
                                  }
                                , Nav.pushUrl
                                    model.key
                                    (excerptToRoute newBooks newExcerpt)
                                )
                        )
                        |> Update.andThen update RequestEmbeddings

        PendingTitleBlur ->
            case model.page of
                CreatePage pExcerpt titles authors ->
                    case
                        model.books
                            |> values
                            |> filter (\book -> book.title == pExcerpt.title)
                            |> head
                    of
                        Just book ->
                            ( { model
                                | page =
                                    CreatePage
                                        { pExcerpt
                                            | author =
                                                withDefault
                                                    ""
                                                    (head book.authors)
                                        }
                                        titles
                                        authors
                              }
                            , none
                            )

                        _ ->
                            noOp

                _ ->
                    noOp
