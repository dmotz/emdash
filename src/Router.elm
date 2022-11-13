module Router exposing
    ( Route(..)
    , authorToRoute
    , deslugify
    , entryToRoute
    , routeParser
    , searchToRoute
    , slugify
    , tagToRoute
    , titleSlugToRoute
    )

import Dict exposing (get)
import Model exposing (Author, BookMap, Entry, Id, Tag, Title)
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
import Utils exposing (rx)


type Route
    = RootRoute
    | TitleRoute Title (Maybe String)
    | EntryRoute Title Id
    | AuthorRoute Author
    | TagRoute Tag
    | SearchRoute (Maybe String)
    | SettingsRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map RootRoute top
        , map TitleRoute (s "title" </> string </> fragment identity)
        , map EntryRoute (s "title" </> string </> string)
        , map AuthorRoute (s "author" </> string)
        , map TagRoute (s "tag" </> string)
        , map SearchRoute (s "search" <?> Query.string "q")
        , map SettingsRoute (s "settings")
        ]


entryToRoute : BookMap -> Entry -> String
entryToRoute books entry =
    case get entry.bookId books of
        Just book ->
            absolute [ "title", slugify book.title, entry.id ] []

        _ ->
            ""


titleSlugToRoute : Title -> String
titleSlugToRoute title =
    absolute [ "title", slugify title ] []


authorToRoute : Author -> String
authorToRoute author =
    absolute [ "author", slugify author ] []


tagToRoute : Tag -> String
tagToRoute tag =
    absolute [ "tag", slugify tag ] []


searchToRoute : String -> String
searchToRoute query =
    absolute [ "search" ] [ Url.Builder.string "q" query ]


slugify : String -> String
slugify =
    replace (rx "\\s") (always "-")
        >> replace (rx "[^\\w-]") (always "")


deslugify : String -> String
deslugify =
    replace (rx "-") (always " ")
