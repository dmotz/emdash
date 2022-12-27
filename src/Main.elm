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
import Json.Decode as Decode
import List
    exposing
        ( concat
        , concatMap
        , drop
        , filter
        , filterMap
        , foldl
        , head
        , isEmpty
        , length
        , map
        , member
        , sortBy
        , take
        )
import Maybe exposing (andThen, withDefault)
import Model
    exposing
        ( BookSort(..)
        , EntrySort(..)
        , Id
        , InputFocus(..)
        , Model
        , Page(..)
        , ScorePairs
        , StoredModel
        , TagSort(..)
        , initialStoredModel
        )
import Msg exposing (Msg(..))
import Parser
import Platform.Cmd exposing (batch, none)
import Random exposing (generate)
import Router
    exposing
        ( Route(..)
        , entryToRoute
        , routeParser
        , searchToRoute
        , slugify
        )
import Set exposing (diff, toList, union)
import String exposing (fromInt, join, toLower, trim)
import Task exposing (attempt, perform)
import Update.Extra as Update exposing (addCmd)
import Url exposing (Url)
import Url.Parser exposing (parse)
import Utils
    exposing
        ( KeyEvent
        , dedupe
        , embeddingBatchSize
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


port handleNewEntries : StoredModel -> Cmd msg


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


appName : String
appName =
    "Marginalia"


maxSearchResults : Int
maxSearchResults =
    100


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.soon 999
    , transform = DebounceMsg
    }


createModel : Maybe StoredModel -> Bool -> Url -> Nav.Key -> Model
createModel maybeStoredModel demoMode url key =
    let
        restored =
            withDefault initialStoredModel maybeStoredModel

        ( titleRouteMap, booksWithSlugs ) =
            Parser.getTitleRouteMap restored.books

        books =
            Dict.fromList (map (juxt .id identity) booksWithSlugs)

        tags =
            restored.books |> map .tags |> concat |> dedupe
    in
    { page = MainPage (values books) Nothing
    , demoMode = demoMode
    , entries = Dict.fromList (map (juxt .id identity) restored.entries)
    , books = books
    , semanticThreshold = 0.1
    , neighborMap = Dict.empty
    , bookNeighborMap = Dict.empty
    , semanticRankMap = Dict.empty
    , hiddenEntries = Set.fromList restored.hiddenEntries
    , completedEmbeddings = Set.empty
    , embeddingsReady = False
    , tags = restored.books |> map .tags |> concat |> dedupe
    , tagCounts = getTagCounts books
    , tagSort = TagAlphaSort
    , showTagHeader = length tags > 0
    , titleRouteMap = titleRouteMap
    , authorRouteMap = Parser.getAuthorRouteMap restored.books
    , pendingTag = Nothing
    , isDragging = False
    , reverseSort = True
    , inputFocused = Nothing
    , parsingError = False
    , url = url
    , key = key
    , bookSort = RecencySort
    , entrySort = EntryPageSort
    , bookmarks = restored.bookmarks |> Dict.fromList
    , idToShowDetails = Dict.empty
    , idToActiveTab = Dict.empty
    , searchQuery = ""
    , searchDebounce = Debounce.init
    }


main : Program (Maybe String) Model Msg
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

                                EntryPage entry book ->
                                    book.title
                                        ++ " "
                                        ++ "p. "
                                        ++ fromInt entry.page

                                SettingsPage ->
                                    "Settings"

                                ImportPage ->
                                    "Import"

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


init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init mStateString url key =
    let
        model =
            createModel
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
        |> addCmd (model |> modelToStoredModel |> handleNewEntries)


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
                    createModel maybeModel demoMode model.url model.key
            in
            update (UrlChanged model.url) model_
                |> addCmd (model_ |> modelToStoredModel |> handleNewEntries)

        ParseJsonText text ->
            case decodeStoredModel text of
                Ok storedModel ->
                    update
                        (RestoreState (Just storedModel) model.demoMode)
                        model

                _ ->
                    noOp

        DragEnter ->
            ( { model | isDragging = True }, none )

        DragLeave ->
            ( { model | isDragging = False }, none )

        GotFiles msg file _ ->
            ( { model | isDragging = False }
            , perform msg (File.toString file)
            )

        PickFile ->
            ( model, Select.files [ "text/plain" ] (GotFiles FileLoad) )

        FileLoad text ->
            ( model, requestUnicodeNormalized text )

        ReceiveUnicodeNormalized text ->
            let
                ( newEntries, newBooks ) =
                    Parser.process text

                hiddenPred =
                    \id _ -> not <| Set.member id model.hiddenEntries

                unseenEntries =
                    Dict.diff newEntries model.entries |> Dict.filter hiddenPred

                bookVals =
                    unseenEntries
                        |> Dict.foldl
                            (\_ entry acc ->
                                Dict.update
                                    entry.bookId
                                    (Maybe.map
                                        (\book ->
                                            { book
                                                | count = book.count + 1
                                                , sortIndex =
                                                    max
                                                        book.sortIndex
                                                        entry.date
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
            if Dict.isEmpty newEntries then
                ( { model | parsingError = True }, none )

            else
                ( { model
                    | parsingError = False
                    , entries =
                        Dict.union model.entries newEntries
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
                    |> addCmd (model |> modelToStoredModel |> handleNewEntries)
                    |> store

        ResetError ->
            ( { model | parsingError = False }, none )

        ShowRandom ->
            ( model
            , generate
                GotRandomIndex
                (Random.int 0 ((model.entries |> values |> length) - 1))
            )

        GotRandomIndex n ->
            case model.entries |> values |> drop n |> head of
                Just entry ->
                    ( model
                    , Nav.pushUrl model.key (entryToRoute model.books entry)
                    )

                _ ->
                    noOp

        SetInputFocus focus ->
            ( { model | inputFocused = focus }, none )

        UpdateNotes id text ->
            store
                ( { model
                    | entries =
                        Dict.update
                            id
                            (Maybe.map (\entry -> { entry | notes = text }))
                            model.entries
                    , page =
                        case model.page of
                            EntryPage entry book ->
                                EntryPage { entry | notes = text } book

                            TitlePage book entries ->
                                TitlePage
                                    book
                                    (map
                                        (\entry ->
                                            if entry.id == id then
                                                { entry | notes = text }

                                            else
                                                entry
                                        )
                                        entries
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
                TitlePage book entries ->
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
                                                entries
                                      }
                                    , none
                                    )

                        _ ->
                            noOp

                _ ->
                    noOp

        RemoveTag tag ->
            case model.page of
                TitlePage book entries ->
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
                                    entries
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
                            TitlePage _ entries ->
                                TitlePage newBook entries

                            _ ->
                                model.page
                  }
                , none
                )

        SetTagSort sort ->
            ( { model | tagSort = sort }, none )

        HideEntry id ->
            let
                entries =
                    remove id model.entries

                books =
                    withDefault
                        model.books
                        (get id model.entries
                            |> Maybe.map
                                (\{ bookId } ->
                                    Dict.update
                                        bookId
                                        (Maybe.map
                                            (\book ->
                                                { book
                                                    | count = book.count - 1
                                                }
                                            )
                                        )
                                        model.books
                                )
                        )
            in
            store
                ( { model
                    | hiddenEntries = Set.insert id model.hiddenEntries
                    , entries = entries
                    , books = books
                    , page =
                        case model.page of
                            TitlePage oldBook oldEntries ->
                                TitlePage
                                    (withDefault oldBook (get oldBook.id books))
                                    (filter
                                        (\entry -> entry.id /= id)
                                        oldEntries
                                    )

                            _ ->
                                model.page
                  }
                , batch
                    [ deleteEmbedding id
                    , case model.page of
                        EntryPage _ _ ->
                            Nav.pushUrl model.key "/"

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
            , Select.files [ "application/json" ] (GotFiles ParseJsonText)
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
                update
                    (case key of
                        "r" ->
                            ShowRandom

                        "s" ->
                            Sort

                        _ ->
                            NoOp
                    )
                    model

        ExportEpub ->
            ( model, Epub.export (values model.books) (values model.entries) )

        RequestEmbeddings ->
            let
                nextBatch =
                    diff
                        (diff
                            (model.entries |> keys |> Set.fromList)
                            model.completedEmbeddings
                        )
                        model.hiddenEntries
                        |> toList
                        |> filterMap (\id -> get id model.entries)
                        |> map (\entry -> ( entry.id, entry.text ))
                        |> take embeddingBatchSize
            in
            if isEmpty nextBatch then
                ( model
                , model.books
                    |> values
                    |> map
                        (\{ id } ->
                            ( id
                            , model.entries
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
                    requestBookNeighbors book.id

                EntryPage entry _ ->
                    requestExcerptNeighbors ( entry.id, True )

                SearchPage query _ _ _ _ ->
                    requestSemanticSearch ( query, model.semanticThreshold )

                _ ->
                    none
            )

        ReceiveNeighbors ( targetId, idScores ) ->
            if Dict.member targetId model.entries then
                ( { model
                    | neighborMap =
                        insert
                            targetId
                            (filterMap
                                (\( id, score ) ->
                                    case get id model.entries of
                                        Just entry ->
                                            Just ( entry.id, score )

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
                SearchPage query _ books entries _ ->
                    ( { model
                        | page =
                            SearchPage
                                query
                                True
                                books
                                entries
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
                                                entries
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
                    if book.id == bookId && model.entrySort == EntrySemanticSort then
                        update (SortEntries model.entrySort) model_

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
                                entries =
                                    model.entries
                                        |> Dict.filter
                                            (\_ { bookId } ->
                                                bookId == book.id
                                            )
                                        |> values
                                        |> sortBy .page
                            in
                            ( { model_
                                | page = TitlePage book entries
                                , entrySort = EntryPageSort
                              }
                            , batch
                                ((case mFragment of
                                    Just entryId ->
                                        attempt
                                            ScrollToElement
                                            (getElement entryId)

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
                                                , map .id entries
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

                Just (EntryRoute titleSlug entrySlug) ->
                    let
                        mEntry =
                            get entrySlug model.entries

                        mBook =
                            get titleSlug model.titleRouteMap
                                |> andThen (\id -> get id model.books)
                    in
                    case ( mEntry, mBook ) of
                        ( Just entry, Just book ) ->
                            ( { model_ | page = EntryPage entry book }
                            , batch
                                [ scrollTop
                                , if model.embeddingsReady then
                                    requestExcerptNeighbors ( entry.id, True )

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

                _ ->
                    ( { model_ | page = NotFoundPage "Route not found." }
                    , none
                    )

        SortBooks sort ->
            ( { model | bookSort = sort, reverseSort = sort /= TitleSort }
            , none
            )

        SortEntries sort ->
            ( { model
                | entrySort = sort
                , page =
                    case model.page of
                        TitlePage book entries ->
                            TitlePage
                                book
                                (case sort of
                                    EntrySemanticSort ->
                                        case get book.id model.semanticRankMap of
                                            Just ids ->
                                                filterMap
                                                    (\( id, _ ) ->
                                                        get id model.entries
                                                    )
                                                    ids

                                            _ ->
                                                sortBy .page entries

                                    _ ->
                                        sortBy .page entries
                                )

                        _ ->
                            model.page
              }
            , none
            )

        SetBookmark bookId entryId ->
            store
                ( { model
                    | bookmarks =
                        case get bookId model.bookmarks of
                            Just prevEntryId ->
                                if prevEntryId == entryId then
                                    remove bookId model.bookmarks

                                else
                                    insert bookId entryId model.bookmarks

                            _ ->
                                insert bookId entryId model.bookmarks
                  }
                , none
                )

        ToggleFavorite id ->
            let
                toggle =
                    \entry -> { entry | isFavorite = not entry.isFavorite }

                newEntries =
                    Dict.update
                        id
                        (Maybe.map toggle)
                        model.entries

                countDelta =
                    if
                        newEntries
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
                    | entries = newEntries
                    , books =
                        case get id newEntries of
                            Just entry ->
                                Dict.update
                                    entry.bookId
                                    (Maybe.map updateCount)
                                    model.books

                            _ ->
                                model.books
                    , page =
                        case model.page of
                            EntryPage entry book ->
                                EntryPage (toggle entry) (updateCount book)

                            TitlePage book entries ->
                                TitlePage
                                    (updateCount book)
                                    (map
                                        (\entry ->
                                            if entry.id == id then
                                                toggle entry

                                            else
                                                entry
                                        )
                                        entries
                                    )

                            _ ->
                                model.page
                  }
                , none
                )

        SetEntryTab id tab toggle ->
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
                    val |> toLower |> trim
            in
            if String.isEmpty query then
                noOp

            else
                ( { model
                    | page =
                        SearchPage
                            query
                            False
                            (findMatches
                                query
                                (\b -> b.title ++ " " ++ join " " b.authors)
                                (values model.books)
                            )
                            (model.entries
                                |> values
                                |> findMatches query .text
                                |> take maxSearchResults
                            )
                            []
                  }
                , if String.length query >= 5 then
                    requestSemanticSearch ( query, model.semanticThreshold )

                  else
                    none
                )

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
