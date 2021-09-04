module Router exposing
    ( Route(..)
    , authorToRoute
    , deslugify
    , entryToRoute
    , routeParser
    , titleToRoute
    )

import Model exposing (Author, Entry, Title)
import Regex
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string)
import Url.Parser.Query as Query
import Utils exposing (rx)


type Route
    = TitleRoute String
    | EntryRoute String String
    | AuthorRoute String
    | TagRoute String
    | TextRoute (Maybe String)
    | SettingsRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map TitleRoute (s "title" </> string)
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


slugify : String -> String
slugify =
    Regex.replace (rx "\\s") (always "-")


deslugify : String -> String
deslugify =
    Regex.replace (rx "-") (always " ")
