port module Epub exposing (export)

import List exposing (concat, filter, indexedMap, map)
import Model exposing (Author, Entry, Title)
import String exposing (fromInt, replace, toLower)


type alias Epub =
    List ( String, String )


port createEpub : Epub -> Cmd msg


globalTitle : String
globalTitle =
    "Marginalia Excerpts"


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


slugifyTitle : Int -> Title -> String
slugifyTitle n title =
    fromInt (n + 1) ++ "_" ++ replace " " "-" (toLower title) ++ ".xhtml"


tocEntry : Int -> ( Title, Author ) -> String
tocEntry i ( title, author ) =
    "<li><a href=\""
        ++ slugifyTitle i title
        ++ "\">"
        ++ title
        ++ "<small>"
        ++ author
        ++ "</small></a></li>"


generateToc : List ( Title, Author ) -> ( String, String )
generateToc pairs =
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
        ++ (String.concat <| indexedMap tocEntry pairs)
        ++ """
          </ol>
        </nav>
      </body>
      </html>
      """
    )


generateContent : List Title -> ( String, String )
generateContent titles =
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

          <dc:identifier id="BookId">546d2ec6-f6af-4eaa-95a4-5e829d0bdb90</dc:identifier>
          <meta refines="#BookId" property="identifier-type" scheme="onix:codelist5">22</meta>
          <meta property="dcterms:identifier" id="meta-identifier">BookId</meta>
          <dc:title>"""
        ++ globalTitle
        ++ """</dc:title>
          <meta property="dcterms:title" id="meta-title">"""
        ++ globalTitle
        ++ """</meta>
          <dc:language>en</dc:language>
          <meta property="dcterms:language" id="meta-language">en</meta>
          <meta name="generator" content="Marginalia"/>
        </metadata>

        <manifest>
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
                            ++ slugifyTitle i title
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
                        "<itemref idref=\"content_" ++ n ++ "_item_" ++ n ++ "\"/>"
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


getAuthor : Title -> List Entry -> String
getAuthor title entries =
    case entries of
        entry :: ents ->
            if entry.title == title then
                entry.author

            else
                getAuthor title ents

        [] ->
            ""


generateChapter : Int -> Title -> Author -> List Entry -> ( String, String )
generateChapter i title author entries =
    ( "OEBPS/" ++ slugifyTitle i title
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
        ++ """</h1>
        <h2>"""
        ++ author
        ++ """</h2>
        """
        ++ (String.concat <|
                map
                    (\entry ->
                        "<p>" ++ replace "&" "&amp;" entry.text ++ "</p>"
                    )
                    entries
           )
        ++ """
      </body>
    </html>
    """
    )


export : List Title -> List Entry -> Cmd msg
export titles entries =
    let
        titleAuthorPairs =
            map (\title -> ( title, getAuthor title entries )) titles
    in
    createEpub <|
        concat
            [ [ container
              , generateToc titleAuthorPairs
              , generateContent titles
              ]
            , indexedMap
                (\i ( title, author ) ->
                    generateChapter
                        i
                        title
                        author
                        (filter (\ent -> ent.title == title) entries)
                )
                titleAuthorPairs
            ]
