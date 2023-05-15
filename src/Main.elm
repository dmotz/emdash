module Main exposing (main)

import Browser exposing (application)
import Browser.Dom exposing (getElement, setViewport)
import Browser.Navigation as Nav
import CsvParser
import Debounce
import Dict exposing (get, insert, keys, remove, values)
import Epub
import File
import File.Select as Select
import Http
import Json.Decode as Decode
import JsonParser exposing (decodeStoredModel)
import KindleParser
import List
    exposing
        ( all
        , concatMap
        , drop
        , filter
        , filterMap
        , foldl
        , head
        , indexedMap
        , isEmpty
        , length
        , map
        , map2
        , member
        , sort
        , sortBy
        , take
        )
import Maybe exposing (andThen, withDefault)
import Model exposing (ModalMsg(..), Model)
import Msg exposing (Msg(..))
import Platform.Cmd exposing (batch, none)
import Ports exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Router
    exposing
        ( Route(..)
        , excerptToRoute
        , routeParser
        , searchToRoute
        , titleSlugToRoute
        )
import Set exposing (diff, toList, union)
import String exposing (fromInt, join, toLower, trim)
import Task exposing (attempt, perform)
import Time exposing (posixToMillis)
import Types
    exposing
        ( BookSort(..)
        , ExcerptSort(..)
        , ExcerptTab(..)
        , Lens(..)
        , Page(..)
        , PendingExcerpt
        , SearchMode(..)
        , StoredModel
        , TagSort(..)
        )
import Update.Extra as Update exposing (addCmd)
import Url exposing (Url, percentEncode)
import Url.Parser exposing (parse)
import Utils
    exposing
        ( appName
        , countLabel
        , dedupe
        , defaultSemanticThreshold
        , delay
        , excerptCountLabel
        , fetchLensText
        , findMatches
        , getAuthorRouteMap
        , getAuthors
        , getCounts
        , getTagCounts
        , getTitleRouteMap
        , insertOnce
        , lensToString
        , makeExcerpt
        , modelToStoredModel
        , removeItem
        , slugify
        , toDict
        , untaggedKey
        , upsert
        )
import Views.Base exposing (view)
import Views.Landing exposing (landingPageBooks)


minSemanticQueryLen : Int
minSemanticQueryLen =
    5


embeddingBatchSize : Int
embeddingBatchSize =
    10


excerptNeighborK : Int
excerptNeighborK =
    5


bookNeighborK : Int
bookNeighborK =
    6


authorNeighborK : Int
authorNeighborK =
    5


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.soon 999
    , transform = DebounceMsg
    }


mimeTxt : String
mimeTxt =
    "text/plain"


mimeCsv : String
mimeCsv =
    "text/csv"


mimeJson : String
mimeJson =
    "application/json"


demoJsonPath : String
demoJsonPath =
    "/demo/demo.json"


createModel :
    Maybe StoredModel
    -> List String
    -> ( String, String, String )
    -> Bool
    -> Url
    -> Nav.Key
    -> Model
createModel mStoredModel supportIssues ( version, mailingListUrl, mailingListField ) demoMode url key =
    let
        restored =
            withDefault
                { excerpts = []
                , books = []
                , hiddenExcerpts = []
                , bookmarks = []
                , semanticThreshold = defaultSemanticThreshold
                , version = ""
                , didJoinMailingList = False
                }
                mStoredModel

        ( titleRouteMap, booksWithSlugs ) =
            getTitleRouteMap restored.books

        books =
            toDict booksWithSlugs

        tags =
            restored.books |> concatMap .tags |> dedupe

        ( exCount, favCount ) =
            getCounts restored.excerpts
    in
    { page = MainPage (values books) Nothing
    , demoMode = demoMode
    , excerpts = toDict restored.excerpts
    , books = books
    , semanticThreshold = restored.semanticThreshold
    , neighborMap = Dict.empty
    , bookNeighborMap = Dict.empty
    , authorNeighborMap = Dict.empty
    , semanticRankMap = Dict.empty
    , hiddenExcerpts = Set.fromList restored.hiddenExcerpts
    , completedEmbeddings = Set.empty
    , embeddingsReady = False
    , authorEmbeddingsReady = False
    , tags = tags
    , tagCounts = getTagCounts books
    , tagSort = TagAlphaSort
    , showTagHeader = length tags > 0
    , titleRouteMap = titleRouteMap
    , authorRouteMap = getAuthorRouteMap restored.books
    , excerptCountMap = exCount
    , favCountMap = favCount
    , pendingTag = Nothing
    , isDragging = False
    , reverseSort = True
    , modalMessage = Nothing
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
    , mailingListEmail = ""
    , mailingListUrl = mailingListUrl
    , mailingListField = mailingListField
    , didJoinMailingList = restored.didJoinMailingList
    , supportIssues = supportIssues
    , showHoverUi = False
    }


main : Program ( Maybe String, List String, ( String, String, String ) ) Model Msg
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

                        LandingPage _ _ ->
                            appName ++ " — organize your book highlights with AI"

                        _ ->
                            (case m.page of
                                MainPage _ (Just tag) ->
                                    "#" ++ tag

                                SearchPage query _ _ _ _ ->
                                    "🔍 " ++ query

                                TitlePage book _ _ ->
                                    book.title

                                AuthorPage author _ ->
                                    author

                                ExcerptPage excerpt book ->
                                    book.title ++ " p. " ++ fromInt excerpt.page

                                SettingsPage ->
                                    "Settings"

                                ImportPage ->
                                    "Import"

                                MonkPage ->
                                    "Monk-Mode"

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
            always <|
                Sub.batch
                    [ receiveExcerptNeighbors ReceiveNeighbors
                    , receiveBookNeighbors ReceiveBookNeighbors
                    , receiveAuthorNeighbors ReceiveAuthorNeighbors
                    , receiveExcerptEmbeddings ReceiveEmbeddings
                    , receiveBookEmbeddings (always ReceiveBookEmbeddings)
                    , receiveAuthorEmbeddings (always ReceiveAuthorEmbeddings)
                    , receiveUnicodeNormalized ReceiveUnicodeNormalized
                    , receiveSemanticSearch ReceiveSemanticSearch
                    , receiveSemanticRank ReceiveSemanticRank
                    , syncState SyncState
                    ]
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init :
    ( Maybe String, List String, ( String, String, String ) )
    -> Url
    -> Nav.Key
    -> ( Model, Cmd Msg )
init ( mStateString, supportIssues, params ) url key =
    let
        model =
            createModel
                (case decodeStoredModel (withDefault "" mStateString) of
                    Ok storedModel ->
                        Just storedModel

                    _ ->
                        Nothing
                )
                supportIssues
                params
                False
                url
                key
    in
    update (UrlChanged url) model
        |> (if Dict.isEmpty model.excerpts then
                identity

            else
                addCmd (model |> modelToStoredModel |> handleNewExcerpts)
           )


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
                        maybeModel
                        model.supportIssues
                        ( model.version
                        , model.mailingListUrl
                        , model.mailingListField
                        )
                        demoMode
                        model.url
                        model.key
            in
            update (UrlChanged model.url) model_
                |> addCmd
                    (batch
                        [ model_ |> modelToStoredModel |> initWithClear
                        , if demoMode then
                            setDemoEmbeddings (keys model_.excerpts)

                          else
                            model_ |> modelToStoredModel |> setStorage
                        ]
                    )
                |> addCmd (Nav.pushUrl model.key "/")

        ParseJsonText forceDemoExit text ->
            let
                demoMode =
                    if forceDemoExit then
                        False

                    else
                        model.demoMode
            in
            case decodeStoredModel text of
                Ok storedModel ->
                    update
                        (RestoreState (Just storedModel) demoMode)
                        { model
                            | modalMessage =
                                Just <|
                                    InfoMsg <|
                                        "Restored "
                                            ++ (storedModel.excerpts
                                                    |> length
                                                    |> excerptCountLabel
                                               )
                                            ++ "."
                            , demoMode = demoMode
                            , completedEmbeddings = Set.empty
                        }

                Err e ->
                    ( { model
                        | modalMessage = Just <| ErrMsg (Decode.errorToString e)
                      }
                    , none
                    )

        ParseCsvText text ->
            case CsvParser.parse text of
                Ok ( excerpts, books ) ->
                    update (MergeNewExcerpts excerpts books) model
                        |> addCmd (Nav.pushUrl model.key "/")

                Err err ->
                    ( { model | modalMessage = Just <| ErrMsg err }, none )

        SyncState sModel ->
            let
                ( exCount, favCount ) =
                    getCounts sModel.excerpts
            in
            ( { model
                | excerpts = toDict sModel.excerpts
                , books = toDict sModel.books
                , hiddenExcerpts = Set.fromList sModel.hiddenExcerpts
                , bookmarks = Dict.fromList sModel.bookmarks
                , excerptCountMap = exCount
                , favCountMap = favCount
              }
            , none
            )
                |> Update.andThen update (UrlChanged model.url)

        DragEnter ->
            ( { model | isDragging = True }, none )

        DragLeave ->
            ( { model | isDragging = False }, none )

        GotFile msg file ->
            ( { model | isDragging = False }
            , perform msg (File.toString file)
            )

        GotDroppedFile file ->
            let
                mime =
                    File.mime file

                mMsg =
                    if mime == mimeTxt then
                        Just LoadKindleFile

                    else if mime == mimeCsv then
                        Just ParseCsvText

                    else if mime == mimeJson then
                        Just <| ParseJsonText True

                    else
                        Nothing
            in
            case mMsg of
                Just msg ->
                    update (GotFile msg file) model

                _ ->
                    ( { model
                        | modalMessage =
                            Just <|
                                ErrMsg <|
                                    "Unsupported file type ("
                                        ++ mime
                                        ++ ")"
                      }
                    , none
                    )

        PickKindleFile ->
            ( model, Select.file [ mimeTxt ] (GotFile LoadKindleFile) )

        LoadKindleFile text ->
            ( model, requestUnicodeNormalized text )

        ReceiveUnicodeNormalized text ->
            let
                ( excerpts, books ) =
                    KindleParser.parse text
            in
            if Dict.isEmpty excerpts then
                ( { model
                    | modalMessage = Just <| ErrMsg "Failed to parse text."
                  }
                , none
                )

            else
                update (MergeNewExcerpts excerpts books) model

        MergeNewExcerpts newExcerpts newBooks ->
            let
                hiddenPred =
                    \id _ -> not <| Set.member id model.hiddenExcerpts

                unseenExcerpts =
                    if model.demoMode then
                        newExcerpts

                    else
                        Dict.diff newExcerpts model.excerpts
                            |> Dict.filter hiddenPred

                bookVals =
                    unseenExcerpts
                        |> Dict.foldl
                            (\_ excerpt acc ->
                                Dict.update
                                    excerpt.bookId
                                    (Maybe.map
                                        (\book ->
                                            { book
                                                | sortIndex =
                                                    max
                                                        book.sortIndex
                                                        excerpt.date
                                            }
                                        )
                                    )
                                    acc
                            )
                            (if model.demoMode then
                                newBooks

                             else
                                Dict.union model.books newBooks
                            )
                        |> values

                ( titleRouteMap, booksWithSlugs ) =
                    getTitleRouteMap bookVals

                excerpts =
                    if model.demoMode then
                        newExcerpts

                    else
                        Dict.union model.excerpts newExcerpts
                            |> Dict.filter hiddenPred

                ( exCount, favCount ) =
                    excerpts |> values |> getCounts
            in
            store
                ( { model
                    | demoMode = False
                    , modalMessage =
                        let
                            n =
                                Dict.size unseenExcerpts
                        in
                        Just <|
                            InfoMsg <|
                                countLabel "new excerpt" n
                                    ++ " imported."
                    , excerpts = excerpts
                    , books =
                        booksWithSlugs
                            |> filter
                                (\{ id } ->
                                    get id exCount |> withDefault 0 |> (/=) 0
                                )
                            |> toDict
                    , excerptCountMap = exCount
                    , favCountMap = favCount
                    , titleRouteMap = titleRouteMap
                    , authorRouteMap =
                        getAuthorRouteMap bookVals
                    , embeddingsReady = False
                    , neighborMap = Dict.empty
                    , completedEmbeddings =
                        if model.demoMode then
                            Set.empty

                        else
                            model.completedEmbeddings
                  }
                , batch
                    [ model
                        |> modelToStoredModel
                        |> (if model.demoMode then
                                initWithClear

                            else
                                handleNewExcerpts
                           )
                    , Nav.pushUrl model.key "/"
                    ]
                )

        ClearModal ->
            ( { model | modalMessage = Nothing }, none )

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

                            TitlePage book excerpts _ ->
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
                                    False

                            _ ->
                                model.page
                  }
                , none
                )

        UpdateBookNotes id text ->
            let
                f =
                    \book -> { book | notes = text }
            in
            store
                ( { model
                    | books = Dict.update id (Maybe.map f) model.books
                    , page =
                        case model.page of
                            TitlePage book excerpts editMode ->
                                TitlePage (f book) excerpts editMode

                            _ ->
                                model.page
                  }
                , none
                )

        UpdatePendingTag text ->
            ( { model | pendingTag = Just text }, none )

        AddTag ->
            case model.page of
                TitlePage book excerpts _ ->
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
                                                False
                                      }
                                    , none
                                    )

                        _ ->
                            noOp

                _ ->
                    noOp

        RemoveTag tag ->
            case model.page of
                TitlePage book excerpts _ ->
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
                                    False
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
                            TitlePage _ excerpts _ ->
                                TitlePage newBook excerpts False

                            _ ->
                                model.page
                  }
                , none
                )

        SetTagSort sort ->
            ( { model | tagSort = sort }, none )

        DeleteExcerpt excerpt ->
            let
                newExcerpts =
                    remove excerpt.id model.excerpts

                ( books, bookmarks ) =
                    case get excerpt.bookId model.excerptCountMap of
                        Just n ->
                            if n == 1 then
                                ( remove excerpt.bookId model.books
                                , remove excerpt.bookId model.bookmarks
                                )

                            else
                                ( model.books
                                , case get excerpt.bookId model.bookmarks of
                                    Just id ->
                                        if id == excerpt.id then
                                            remove excerpt.bookId model.bookmarks

                                        else
                                            model.bookmarks

                                    _ ->
                                        model.bookmarks
                                )

                        _ ->
                            ( model.books, model.bookmarks )

                ( exCount, favCount ) =
                    newExcerpts |> values |> getCounts
            in
            store
                ( { model
                    | hiddenExcerpts = Set.insert excerpt.id model.hiddenExcerpts
                    , excerpts = newExcerpts
                    , books = books
                    , bookmarks = bookmarks
                    , excerptCountMap = exCount
                    , favCountMap = favCount
                    , page =
                        case model.page of
                            TitlePage oldBook oldExcerpts _ ->
                                TitlePage
                                    (withDefault oldBook (get oldBook.id books))
                                    (filter
                                        (\e -> e.id /= excerpt.id)
                                        oldExcerpts
                                    )
                                    False

                            _ ->
                                model.page
                    , tagCounts = getTagCounts books
                    , completedEmbeddings =
                        Set.remove excerpt.id model.completedEmbeddings
                    , neighborMap = Dict.empty
                    , bookNeighborMap = Dict.empty
                    , modalMessage = Nothing
                  }
                , batch
                    [ deleteExcerpt
                        ( excerpt.id
                        , ( excerpt.bookId
                          , newExcerpts
                                |> Dict.filter
                                    (\_ { bookId } -> bookId == excerpt.bookId)
                                |> values
                                |> map .id
                          )
                        , bookNeighborK
                        )
                    , case model.page of
                        ExcerptPage { id } _ ->
                            if id == excerpt.id then
                                Nav.pushUrl model.key "/"

                            else
                                none

                        TitlePage book ents _ ->
                            if book.id == excerpt.bookId && length ents == 1 then
                                Nav.pushUrl model.key "/"

                            else
                                none

                        _ ->
                            none
                    ]
                )

        DeleteBook book ->
            let
                exIds =
                    model.excerpts
                        |> Dict.filter (\_ { bookId } -> bookId == book.id)
                        |> values
                        |> map .id
                        |> Set.fromList

                tagCounts =
                    foldl
                        (\tag acc ->
                            Dict.update
                                tag
                                (Maybe.map ((+) -1))
                                acc
                        )
                        model.tagCounts
                        book.tags

                excerpts =
                    Dict.filter
                        (\_ { bookId } -> bookId /= book.id)
                        model.excerpts

                ( exCount, favCount ) =
                    excerpts |> values |> getCounts

                newBooks =
                    remove book.id model.books
            in
            store
                ( { model
                    | modalMessage = Nothing
                    , excerpts = excerpts
                    , books = newBooks
                    , excerptCountMap = exCount
                    , favCountMap = favCount
                    , bookNeighborMap = Dict.empty
                    , neighborMap = Dict.empty
                    , authorNeighborMap = Dict.empty
                    , hiddenExcerpts = union model.hiddenExcerpts exIds
                    , completedEmbeddings = diff model.completedEmbeddings exIds
                    , titleRouteMap = remove book.slug model.titleRouteMap
                    , tagCounts = tagCounts
                    , tags = newBooks |> values |> concatMap .tags |> dedupe
                    , bookmarks = remove book.id model.bookmarks
                  }
                , batch
                    [ Nav.pushUrl model.key "/"
                    , deleteBook ( book.id, toList exIds )
                    ]
                )

        EnterBookEditMode ->
            case model.page of
                TitlePage book excerpts _ ->
                    ( { model | page = TitlePage book excerpts True }, none )

                _ ->
                    noOp

        ExitBookEditMode ->
            case model.page of
                TitlePage book excerpts _ ->
                    ( { model
                        | page =
                            TitlePage
                                { book
                                    | title =
                                        withDefault
                                            book.title
                                            (get book.id model.books
                                                |> Maybe.map .title
                                            )
                                }
                                excerpts
                                False
                      }
                    , none
                    )

                _ ->
                    noOp

        SetPendingBookTitle title ->
            case model.page of
                TitlePage book excerpts editMode ->
                    ( { model
                        | page = TitlePage { book | title = title } excerpts editMode
                      }
                    , none
                    )

                _ ->
                    noOp

        SetPendingBookAuthor author ->
            case model.page of
                TitlePage book excerpts editMode ->
                    ( { model
                        | page =
                            TitlePage
                                { book | authors = String.split "/" author }
                                excerpts
                                editMode
                      }
                    , none
                    )

                _ ->
                    noOp

        SetBookEdits ->
            case model.page of
                TitlePage book excerpts _ ->
                    let
                        newBook =
                            { book
                                | title = trim book.title
                                , authors =
                                    book.authors
                                        |> map trim
                                        |> filter (not << String.isEmpty)
                            }
                    in
                    if trim newBook.title == "" then
                        update ExitBookEditMode model

                    else
                        let
                            ( titleRouteMap, booksWithSlugs ) =
                                insert book.id newBook model.books
                                    |> values
                                    |> getTitleRouteMap

                            newBooks =
                                toDict booksWithSlugs
                        in
                        store
                            ( { model
                                | page = TitlePage book excerpts False
                                , books = newBooks
                                , titleRouteMap = titleRouteMap
                              }
                            , Nav.replaceUrl
                                model.key
                                (titleSlugToRoute
                                    (get book.id newBooks
                                        |> Maybe.map .slug
                                        |> withDefault ""
                                    )
                                )
                            )

                _ ->
                    noOp

        ShowConfirmation text action ->
            ( { model | modalMessage = Just <| ConfirmationMsg text action }
            , none
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
            , Select.file [ mimeJson ] (GotFile (ParseJsonText True))
            )

        ImportCsv ->
            ( model
            , Select.file [ mimeCsv ] (GotFile ParseCsvText)
            )

        ExportEpub time ->
            ( model, Epub.export model time )

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

        ReceiveBookEmbeddings ->
            ( { model | embeddingsReady = True }
            , case model.page of
                TitlePage book _ _ ->
                    batch
                        [ requestBookNeighbors ( book.id, bookNeighborK )
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
                    requestExcerptNeighbors ( excerpt.id, excerptNeighborK, True )

                SearchPage query _ _ _ _ ->
                    requestSemanticSearch ( query, model.semanticThreshold )

                AuthorPage _ _ ->
                    model |> getAuthors |> requestAuthorEmbeddings

                _ ->
                    none
            )

        ReceiveAuthorEmbeddings ->
            ( { model | authorEmbeddingsReady = True }
            , case model.page of
                AuthorPage author _ ->
                    requestAuthorNeighbors ( author, authorNeighborK )

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

        ReceiveAuthorNeighbors ( targetAuthor, idScores ) ->
            ( { model
                | authorNeighborMap =
                    insert
                        targetAuthor
                        idScores
                        model.authorNeighborMap
              }
            , none
            )

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
                                                (\excerpt acc ->
                                                    acc
                                                        || excerpt.id
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
                TitlePage book _ _ ->
                    if book.id == bookId && model.excerptSort == ExcerptSemanticSort then
                        update (SortExcerpts model.excerptSort) model_

                    else
                        ( model_, none )

                _ ->
                    ( model_, none )

        SetSemanticThreshold s ->
            case String.toFloat s of
                Just n ->
                    store ( { model | semanticThreshold = n }, none )

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
                    { model
                        | url = url
                        , pendingTag = Nothing
                        , searchQuery = ""
                    }

                scrollTop =
                    perform (always NoOp) (setViewport 0 0)

                showLanding =
                    Dict.isEmpty model.books

                rootRedirect =
                    ( model_, Nav.pushUrl model.key "/" )
            in
            case
                parse routeParser url
            of
                Just RootRoute ->
                    ( { model_
                        | page =
                            if showLanding then
                                LandingPage [] Dict.empty

                            else
                                MainPage (values model.books) Nothing
                      }
                    , batch
                        [ scrollTop
                        , if showLanding then
                            batch
                                [ generate
                                    GotLandingData
                                    (Random.pair
                                        (shuffle landingPageBooks)
                                        (Random.list
                                            (length landingPageBooks)
                                            (Random.int 3 79)
                                        )
                                    )
                                , Http.get
                                    { url = demoJsonPath
                                    , expect = Http.expectWhatever (always NoOp)
                                    }
                                , fetchDemoEmbeddings ()
                                ]

                          else
                            none
                        ]
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
                                        |> sortBy
                                            (\{ page, date } ->
                                                if page == -1 then
                                                    date

                                                else
                                                    page
                                            )
                            in
                            ( { model_
                                | page = TitlePage book excerpts False
                                , excerptSort = ExcerptPageSort
                                , showHoverUi = True
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
                                            [ if not (Dict.member book.id model.bookNeighborMap) then
                                                requestBookNeighbors ( book.id, bookNeighborK )

                                              else
                                                none
                                            , if not (Dict.member book.id model.semanticRankMap) then
                                                requestSemanticRank
                                                    ( book.id
                                                    , map .id excerpts
                                                    )

                                              else
                                                none
                                            ]

                                        else
                                            []
                                       )
                                    ++ [ delay 2000 (HideHoverUiState book.id) ]
                                )
                            )

                        _ ->
                            if showLanding then
                                rootRedirect

                            else
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
                                , if
                                    model.embeddingsReady
                                        && not (Dict.member excerpt.id model.neighborMap)
                                  then
                                    requestExcerptNeighbors ( excerpt.id, excerptNeighborK, True )

                                  else
                                    none
                                , if model.demoMode then
                                    fetchLensText excerpt.id Succint

                                  else
                                    none
                                ]
                            )

                        _ ->
                            if showLanding then
                                rootRedirect

                            else
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
                            , batch
                                [ if model.authorEmbeddingsReady then
                                    if not <| Dict.member author model.authorNeighborMap then
                                        requestAuthorNeighbors ( author, authorNeighborK )

                                    else
                                        none

                                  else if model.embeddingsReady then
                                    model |> getAuthors |> requestAuthorEmbeddings

                                  else
                                    none
                                , scrollTop
                                ]
                            )

                        _ ->
                            if showLanding then
                                rootRedirect

                            else
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
                        , scrollTop
                        )

                    else if showLanding then
                        rootRedirect

                    else
                        ( { model_
                            | page = NotFoundPage "Tag not found."
                          }
                        , scrollTop
                        )

                Just (SearchRoute query) ->
                    if showLanding then
                        rootRedirect

                    else
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
                                    , page =
                                        case model.page of
                                            SearchPage _ _ _ _ _ ->
                                                model.page

                                            _ ->
                                                SearchPage text TextMatches [] [] []
                                  }
                                , cmd
                                )

                            _ ->
                                ( model_, none )

                Just SettingsRoute ->
                    ( { model_ | page = SettingsPage }, scrollTop )

                Just ImportRoute ->
                    ( { model_ | page = ImportPage }, scrollTop )

                Just MonkRoute ->
                    ( { model_ | page = MonkPage }, scrollTop )

                Just (CreateRoute mTitle mAuthor mText mSource mPage) ->
                    let
                        pendingEx =
                            PendingExcerpt
                                (withDefault "" mTitle)
                                (withDefault "" mAuthor)
                                (withDefault "" mText)
                                mPage
                                (withDefault "" mSource)
                    in
                    if
                        all
                            (not << String.isEmpty)
                            [ pendingEx.title, pendingEx.author, pendingEx.text ]
                    then
                        update (GetTime (CreateExcerpt pendingEx)) model

                    else
                        ( { model_
                            | page =
                                CreatePage
                                    pendingEx
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
                        TitlePage book excerpts _ ->
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
                                False

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

        ToggleFavorite excerpt ->
            let
                newExcerpt =
                    { excerpt | isFavorite = not excerpt.isFavorite }
            in
            store
                ( { model
                    | excerpts = insert newExcerpt.id newExcerpt model.excerpts
                    , favCountMap =
                        Dict.update
                            newExcerpt.id
                            (Maybe.map
                                ((+)
                                    (if newExcerpt.isFavorite then
                                        1

                                     else
                                        -1
                                    )
                                )
                            )
                            model.favCountMap
                    , page =
                        case model.page of
                            ExcerptPage _ book ->
                                ExcerptPage newExcerpt book

                            TitlePage book excerpts _ ->
                                TitlePage
                                    book
                                    (map
                                        (\ex ->
                                            if ex.id == newExcerpt.id then
                                                newExcerpt

                                            else
                                                ex
                                        )
                                        excerpts
                                    )
                                    False

                            _ ->
                                model.page
                  }
                , none
                )

        SetExcerptTab excerpt tab toggle ->
            let
                { id } =
                    excerpt
            in
            ( { model
                | idToActiveTab = insert id tab model.idToActiveTab
                , idToShowDetails =
                    if toggle then
                        Dict.update id
                            (withDefault False >> not >> Just)
                            model.idToShowDetails

                    else
                        model.idToShowDetails
              }
            , case tab of
                Related ->
                    if
                        not (Dict.member id model.neighborMap)
                            && model.embeddingsReady
                    then
                        requestExcerptNeighbors
                            ( id, excerptNeighborK, True )

                    else
                        none

                Lenses lens _ ->
                    let
                        lensKey =
                            lensToString lens
                    in
                    if
                        not <|
                            foldl
                                (\( k, v ) acc ->
                                    acc || (k == lensKey && v /= [])
                                )
                                False
                                excerpt.lenses
                    then
                        if model.demoMode then
                            fetchLensText id lens

                        else
                            none

                    else
                        none

                _ ->
                    none
            )

        ScrollToTop ->
            ( model, scrollToTop () )

        HideHoverUiState id ->
            case model.page of
                TitlePage book _ _ ->
                    if book.id == id then
                        ( { model | showHoverUi = False }, none )

                    else
                        noOp

                _ ->
                    noOp

        OnSearchStart query ->
            if String.isEmpty query then
                ( { model | searchQuery = "" }, Nav.back model.key 1 )

            else
                ( { model
                    | searchQuery = query
                    , page =
                        case model.page of
                            SearchPage _ _ _ _ _ ->
                                model.page

                            _ ->
                                SearchPage query TextMatches [] [] []
                  }
                , if String.isEmpty model.searchQuery then
                    Nav.pushUrl model.key (searchToRoute query)

                  else
                    Nav.replaceUrl model.key (searchToRoute query)
                )

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

            else if
                case model.page of
                    SearchPage _ _ _ _ _ ->
                        False

                    _ ->
                        True
            then
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
                                |> findMatches query (\e -> e.text ++ " " ++ e.notes)
                                |> sortBy .bookId
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
            if model.supportIssues == [] then
                ( { model | demoMode = True }
                , Http.get
                    { url = demoJsonPath, expect = Http.expectString GotDemoData }
                )

            else
                ( { model
                    | modalMessage =
                        Just <|
                            InitErrMsg
                                ("Missing support for "
                                    ++ String.join ", " model.supportIssues
                                )
                  }
                , none
                )

        GotDemoData result ->
            case result of
                Ok text ->
                    update (ParseJsonText False text) model

                _ ->
                    noOp

        GotLandingData ( titles, nums ) ->
            case model.page of
                LandingPage _ _ ->
                    let
                        books =
                            indexedMap
                                (\i ( title, author ) ->
                                    { id = String.fromInt i
                                    , title = title
                                    , authors = [ author ]
                                    , rating = 0
                                    , sortIndex = 0
                                    , tags = []
                                    , slug = ""
                                    , notes = ""
                                    }
                                )
                                titles
                    in
                    ( { model
                        | page =
                            LandingPage
                                books
                                (map2 (\{ id } n -> ( id, n )) books nums
                                    |> Dict.fromList
                                )
                      }
                    , none
                    )

                _ ->
                    noOp

        GetTime msg ->
            ( model, perform msg Time.now )

        UpdatePendingExcerpt pExcerpt ->
            case model.page of
                CreatePage _ titles authors ->
                    ( { model | page = CreatePage pExcerpt titles authors }, none )

                _ ->
                    noOp

        CreateExcerpt pendingExcerpt time ->
            let
                ( excerpt, book ) =
                    makeExcerpt
                        pendingExcerpt.title
                        pendingExcerpt.author
                        pendingExcerpt.text
                        pendingExcerpt.page
                        (time |> posixToMillis |> Just)
                        ""
                        (if pendingExcerpt.sourceUrl |> trim |> String.isEmpty then
                            Nothing

                         else
                            pendingExcerpt.sourceUrl |> trim |> Just
                        )
            in
            case get excerpt.id model.excerpts of
                Just existingExcerpt ->
                    ( model
                    , Nav.pushUrl
                        model.key
                        (excerptToRoute model.books existingExcerpt)
                    )

                _ ->
                    let
                        model_ =
                            { model
                                | neighborMap = Dict.empty
                                , bookNeighborMap = Dict.empty
                                , embeddingsReady = False
                                , excerpts =
                                    insert excerpt.id excerpt model.excerpts
                                , excerptCountMap =
                                    upsert model.excerptCountMap book.id ((+) 1) 1
                            }
                    in
                    store
                        (case get book.id model.books of
                            Just _ ->
                                let
                                    newBooks =
                                        Dict.update
                                            book.id
                                            (Maybe.map
                                                (\b ->
                                                    { b
                                                        | sortIndex =
                                                            max
                                                                b.sortIndex
                                                                excerpt.date
                                                    }
                                                )
                                            )
                                            model.books
                                in
                                ( { model_
                                    | books = newBooks
                                    , semanticRankMap =
                                        remove
                                            book.id
                                            model.semanticRankMap
                                    , tagCounts = getTagCounts newBooks
                                  }
                                , Nav.pushUrl
                                    model.key
                                    (excerptToRoute newBooks excerpt)
                                )

                            _ ->
                                let
                                    ( titleRouteMap, booksWithSlugs ) =
                                        insert book.id book model.books
                                            |> values
                                            |> getTitleRouteMap

                                    newBooks =
                                        toDict booksWithSlugs
                                in
                                ( { model_
                                    | books = newBooks
                                    , titleRouteMap = titleRouteMap
                                    , authorRouteMap =
                                        getAuthorRouteMap booksWithSlugs
                                  }
                                , Nav.pushUrl
                                    model.key
                                    (excerptToRoute newBooks excerpt)
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

        UpdateMailingListEmail val ->
            ( { model | mailingListEmail = val }, none )

        SubscribeToMailingList ->
            if String.isEmpty model.mailingListEmail then
                noOp

            else
                store
                    ( { model | didJoinMailingList = True }
                    , Http.post
                        { url = model.mailingListUrl
                        , body =
                            Http.stringBody
                                "application/x-www-form-urlencoded"
                                (model.mailingListField
                                    ++ "="
                                    ++ percentEncode model.mailingListEmail
                                )
                        , expect = Http.expectWhatever (always NoOp)
                        }
                    )

        ReceiveLensText id lensType result ->
            case result of
                Ok lensText ->
                    let
                        lensKey =
                            lensToString lensType

                        f =
                            \excerpt ->
                                { excerpt
                                    | lenses =
                                        ( lensKey, [ lensText ] ) :: excerpt.lenses
                                }
                    in
                    store
                        ( { model
                            | excerpts =
                                Dict.update
                                    id
                                    (Maybe.map f)
                                    model.excerpts
                            , page =
                                case model.page of
                                    ExcerptPage excerpt book ->
                                        ExcerptPage (f excerpt) book

                                    TitlePage book excerpts _ ->
                                        TitlePage
                                            book
                                            (map
                                                (\e ->
                                                    if e.id == id then
                                                        f e

                                                    else
                                                        e
                                                )
                                                excerpts
                                            )
                                            False

                                    _ ->
                                        model.page
                          }
                        , none
                        )

                _ ->
                    noOp
