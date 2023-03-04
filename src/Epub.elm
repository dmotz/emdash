port module Epub exposing (export)

import List exposing (concat, filter, indexedMap, map, sortBy)
import MD5 exposing (hex)
import Model exposing (Author, Book, BookSort(..), Excerpt, Title)
import Regex exposing (Regex)
import String exposing (fromInt, join, padLeft, replace, toLower)
import Time
    exposing
        ( Month(..)
        , Posix
        , toDay
        , toHour
        , toMinute
        , toMonth
        , toSecond
        , toYear
        , utc
        )
import Utils exposing (appName, rx, sortBooks)


type alias Epub =
    List ( String, String )


port createEpub : Epub -> Cmd msg


globalTitle : String
globalTitle =
    appName ++ " Excerpts"


container : ( String, String )
container =
    ( "META-INF/container.xml"
    , """
      <?xml version="1.0" encoding="UTF-8" ?>
      <container
        version="1.0"
        xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
        <rootfiles>
          <rootfile
            full-path="OEBPS/content.opf"
            media-type="application/oebps-package+xml"/>
        </rootfiles>
      </container>
      """
    )


titleRx : Regex
titleRx =
    rx "[^a-zA-Z\\d\\-]"


trimRx : Regex
trimRx =
    rx "[\\s\\n]+"


normalizeTitle : Int -> Title -> String
normalizeTitle n title =
    fromInt (n + 1)
        ++ "_"
        ++ (Regex.replace titleRx (always "") (toLower title)
                |> replace " " "-"
           )
        ++ ".xhtml"


tocExcerpt : Int -> Title -> String
tocExcerpt i title =
    "<li><a href=\"" ++ normalizeTitle i title ++ "\">" ++ title ++ "</a></li>"


generateToc : List Title -> ( String, String )
generateToc titles =
    ( "OEBPS/toc.xhtml"
    , """
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE html>
      <html
        xmlns="http://www.w3.org/1999/xhtml"
        xmlns:epub="http://www.idpf.org/2007/ops"
        xml:lang="en"
        lang="en">
      <head>
        <title>"""
        ++ globalTitle
        ++ """</title>
        <meta charset="UTF-8" />
      </head>
      <body>
        <h1>Table of Contents</h1>
        <nav epub:type="toc">
          <ol>
            <li><a href="toc.xhtml">- Table of Contents -</a></li>
      """
        ++ (String.concat <| indexedMap tocExcerpt titles)
        ++ """
          </ol>
        </nav>
      </body>
      </html>
      """
    )


generateTocNcx : String -> List Title -> ( String, String )
generateTocNcx epubId titles =
    ( "OEBPS/toc.ncx"
    , """
      <?xml version="1.0" encoding="UTF-8"?>
      <ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">
        <head>
          <meta name="dtb:uid" content=\""""
        ++ epubId
        ++ """" />
          <meta name="dtb:generator" content=\""""
        ++ appName
        ++ """"/>
          <meta name="dtb:depth" content="1"/>
          <meta name="dtb:totalPageCount" content="0"/>
          <meta name="dtb:maxPageNumber" content="0"/>
        </head>
        <docTitle>
          <text>"""
        ++ globalTitle
        ++ """</text>
        </docTitle>
        <docAuthor>
          <text>"""
        ++ appName
        ++ """</text>
        </docAuthor>
        <navMap>
          <navPoint id="toc" playOrder="0" class="chapter">
            <navLabel>
              <text>Table of Contents</text>
            </navLabel>
            <content src="toc.xhtml"/>
          </navPoint>
        """
        ++ (String.concat <|
                indexedMap
                    (\i title ->
                        let
                            n =
                                fromInt i

                            n1 =
                                fromInt <| i + 1
                        in
                        """
                        <navPoint id="content_"""
                            ++ n
                            ++ "_item_"
                            ++ n
                            ++ "\" playOrder=\""
                            ++ n1
                            ++ "\">"
                            ++ """
                          <navLabel>
                            <text>"""
                            ++ n1
                            ++ """. """
                            ++ title
                            ++ """</text>
                          </navLabel>
                          <content src=\""""
                            ++ normalizeTitle i title
                            ++ """"/>
                        </navPoint>
                        """
                    )
                    titles
           )
        ++ """
          </navMap>
        </ncx>
  """
    )


generateContent : String -> String -> List Title -> ( String, String )
generateContent epubId timeString titles =
    ( "OEBPS/content.opf"
    , """
      <?xml version="1.0" encoding="UTF-8"?>
      <package
        xmlns="http://www.idpf.org/2007/opf"
        version="3.0"
        unique-identifier="BookId"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:dcterms="http://purl.org/dc/terms/"
        xml:lang="en"
        xmlns:media="http://www.idpf.org/epub/vocab/overlays/#">
        <metadata
          xmlns:dc="http://purl.org/dc/elements/1.1/"
          xmlns:opf="http://www.idpf.org/2007/opf">
          <dc:identifier id="BookId">"""
        ++ epubId
        ++ """</dc:identifier>
          <meta
            refines="#BookId"
            property="identifier-type"
            scheme="onix:codelist5">22</meta>
          <meta property="dcterms:identifier" id="meta-identifier">BookId</meta>
          <meta property="dcterms:modified">"""
        ++ timeString
        ++ """</meta>
          <dc:title>"""
        ++ globalTitle
        ++ """</dc:title>
          <meta property="dcterms:title" id="meta-title">"""
        ++ globalTitle
        ++ """</meta>
          <dc:creator id="creator">"""
        ++ appName
        ++ """</dc:creator>
          <meta refines="#creator" property="file-as">"""
        ++ appName
        ++ """</meta>
          <dc:language>en</dc:language>
          <meta property="dcterms:language" id="meta-language">en</meta>
          <meta name="generator" content=\""""
        ++ appName
        ++ """"/>
        </metadata>
        <manifest>
          <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml" />
          <item
            id="toc"
            href="toc.xhtml"
            media-type="application/xhtml+xml"
            properties="nav"/>
        """
        ++ (String.concat <|
                indexedMap
                    (\i title ->
                        let
                            n =
                                fromInt (i + 1)
                        in
                        "<item id=\"content_"
                            ++ n
                            ++ "_item_"
                            ++ n
                            ++ "\" href=\""
                            ++ normalizeTitle i title
                            ++ "\" media-type=\"application/xhtml+xml\"/>"
                    )
                    titles
           )
        ++ """
        </manifest>
        <spine toc="ncx">
          <itemref idref="toc"/>
        """
        ++ (String.concat <|
                indexedMap
                    (\i _ ->
                        let
                            n =
                                fromInt (i + 1)
                        in
                        "<itemref idref=\"content_"
                            ++ n
                            ++ "_item_"
                            ++ n
                            ++ "\"/>"
                    )
                    titles
           )
        ++ """
        </spine>
        <guide>
          <reference type="text" title="Table of Contents" href="toc.xhtml"/>
        </guide>
      </package>
  """
    )


generateChapter : Int -> Title -> List Author -> List Excerpt -> ( String, String )
generateChapter i title authors excerpts =
    ( "OEBPS/" ++ normalizeTitle i title
    , """
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE html>
    <html
      xmlns="http://www.w3.org/1999/xhtml"
      xmlns:epub="http://www.idpf.org/2007/ops"
      lang="en">
      <head>
        <meta charset="UTF-8" />
        <title>"""
        ++ title
        ++ """</title>
      </head>
      <body>
        <h1>"""
        ++ title
        ++ "</h1>"
        ++ join " " (map (\author -> "<h2>" ++ author ++ "</h2>") authors)
        ++ (String.concat <|
                map
                    (\{ text, page } ->
                        "<p>"
                            ++ (text
                                    |> replace "&" "&amp;"
                                    |> replace "<" "&lt;"
                                    |> replace ">" "&gt;"
                               )
                            ++ "</p>"
                            ++ (if page /= -1 then
                                    "<cite>— p. " ++ fromInt page ++ "</cite>"

                                else
                                    ""
                               )
                    )
                    excerpts
           )
        ++ """
      </body>
    </html>
    """
    )


export : Posix -> List Book -> List Excerpt -> Cmd msg
export time books excerpts =
    let
        sortedBooks =
            sortBooks RecencySort True books

        titles =
            map (.title >> replace "&" "and") sortedBooks

        timeString =
            join
                "-"
                [ toYear utc time |> fromInt
                , toMonth utc time |> monthToInt |> padN
                , toDay utc time |> padN
                ]
                ++ "T"
                ++ join
                    ":"
                    (map
                        (\f -> f utc time |> padN)
                        [ toHour, toMinute, toSecond ]
                    )
                ++ "Z"

        epubId =
            hex timeString
    in
    [ [ container
      , generateToc titles
      , generateTocNcx epubId titles
      , generateContent epubId timeString titles
      ]
    , indexedMap
        (\i { id, title, authors } ->
            generateChapter
                i
                (replace " & " " and " title)
                authors
                (excerpts |> filter (.bookId >> (==) id) |> sortBy .page)
        )
        sortedBooks
    ]
        |> concat
        |> map
            (\( path, text ) ->
                ( path, Regex.replace trimRx (always " ") text )
            )
        |> createEpub


padN : Int -> String
padN =
    fromInt >> padLeft 2 '0'


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
