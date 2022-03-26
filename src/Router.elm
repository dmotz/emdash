module Router exposing
    ( Route(..)
    , authorToRoute
    , deslugify
    , entryToRoute
    , routeParser
    , slugify
    , tagToRoute
    , titleToRoute
    )

import Model exposing (Author, Entry, Tag, Title)
import Regex exposing (replace)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query
import Utils exposing (rx)


type Route
    = RootRoute
    | TitleRoute String
    | EntryRoute String String
    | AuthorRoute String
    | TagRoute String
    | TextRoute (Maybe String)
    | SettingsRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map RootRoute top
        , map TitleRoute (s "title" </> string)
        , map EntryRoute (s "title" </> string </> string)
        , map AuthorRoute (s "author" </> string)
        , map TagRoute (s "tag" </> string)
        , map TextRoute (s "text" <?> Query.string "q")
        , map SettingsRoute (s "settings")
        ]


entryToRoute : Entry -> String
entryToRoute entry =
    absolute [ "title", slugify entry.title, entry.id ] []


titleToRoute : Title -> String
titleToRoute title =
    absolute [ "title", slugify title ] []


authorToRoute : Author -> String
authorToRoute author =
    absolute [ "author", slugify author ] []


tagToRoute : Tag -> String
tagToRoute tag =
    absolute [ "tag", slugify tag ] []


slugify : String -> String
slugify =
    replace (rx "\\s") (always "-")
        >> replace (rx "[^\\w-]") (always "")


deslugify : String -> String
deslugify =
    replace (rx "-") (always " ")
