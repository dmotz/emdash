module Router exposing
    ( Route(..)
    , authorToRoute
    , excerptToRoute
    , routeParser
    , searchToRoute
    , tagToRoute
    , titleSlugToRoute
    )

import Dict exposing (get)
import Model exposing (Author, BookMap, Excerpt, Id, Tag, Title)
import Regex exposing (replace)
import Url.Builder exposing (absolute)
import Url.Parser
    exposing
        ( (</>)
        , (<?>)
        , Parser
        , fragment
        , map
        , oneOf
        , s
        , string
        , top
        )
import Url.Parser.Query as Query
import Utils exposing (slugify)


type Route
    = RootRoute
    | TitleRoute Title (Maybe String)
    | ExcerptRoute Title Id
    | AuthorRoute Author
    | TagRoute Tag
    | SearchRoute (Maybe String)
    | SettingsRoute
    | ImportRoute
    | CreateRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map RootRoute top
        , map TitleRoute (s "title" </> string </> fragment identity)
        , map ExcerptRoute (s "title" </> string </> string)
        , map AuthorRoute (s "author" </> string)
        , map TagRoute (s "tag" </> string)
        , map SearchRoute (s "search" <?> Query.string "q")
        , map SettingsRoute (s "settings")
        , map ImportRoute (s "import")
        , map CreateRoute (s "create")
        ]


excerptToRoute : BookMap -> Excerpt -> String
excerptToRoute books excerpt =
    case get excerpt.bookId books of
        Just book ->
            absolute [ "title", book.slug, excerpt.id ] []

        _ ->
            ""


titleSlugToRoute : String -> String
titleSlugToRoute slug =
    absolute [ "title", slug ] []


authorToRoute : Author -> String
authorToRoute author =
    absolute [ "author", slugify author ] []


tagToRoute : Tag -> String
tagToRoute tag =
    absolute [ "tag", slugify tag ] []


searchToRoute : String -> String
searchToRoute query =
    absolute [ "search" ] [ Url.Builder.string "q" query ]
