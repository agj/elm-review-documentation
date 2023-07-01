module Docs.Utils.Link exposing
    ( FileTarget(..)
    , Link
    , StartsWith(..)
    , SubTarget(..)
    , findLinks
    , parseExternalPackageUrl
    )

import Elm.Project exposing (Project(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import String exposing (startsWith)
import Url exposing (Url)
import Url.Parser exposing ((</>))


addOffset : Int -> Range -> Range
addOffset lineNumber { start, end } =
    { start = addLineNumber lineNumber start
    , end = addLineNumber lineNumber end
    }


addLineNumber : Int -> Location -> Location
addLineNumber lineNumber { row, column } =
    { row = lineNumber + row
    , column = column + 1
    }


type alias Link =
    { file : FileTarget
    , startsWith : StartsWith
    , slug : Maybe String
    }


type FileTarget
    = ModuleTarget ModuleName
    | ReadmeTarget
    | PackagesTarget { name : String, subTarget : SubTarget }
    | External String


type SubTarget
    = VersionsSubTarget
    | ReadmeSubTarget String
    | ModuleSubTarget String ModuleName


type StartsWith
    = StartsWithDotSlash
    | StartsWithSlash
    | StartsWithOther


idParser : Char -> Parser String
idParser endChar =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c /= endChar && c /= ' ')
        |> Parser.getChompedString


onlyModuleNameParser : Parser ModuleName
onlyModuleNameParser =
    Parser.succeed identity
        |= moduleNameParser
        |. Parser.end


moduleNameParser : Parser ModuleName
moduleNameParser =
    manySeparated
        { by = "-"
        , item = moduleNameSegmentParser
        }


moduleNameSegmentParser : Parser String
moduleNameSegmentParser =
    Parser.succeed ()
        |. Parser.chompIf (\c -> Char.isUpper c)
        |. Parser.chompWhile (\c -> Char.isAlphaNum c)
        |> Parser.getChompedString


findLinks : Int -> ModuleName -> String -> List (Node Link)
findLinks row moduleName string =
    string
        |> String.lines
        |> List.indexedMap
            (\lineNumber lineContent ->
                lineContent
                    |> Parser.run (findParser (linkParser (lineNumber + row) moduleName))
                    |> Result.withDefault []
                    |> List.filterMap identity
                    |> List.indexedMap
                        (\index (Node { start, end } link) ->
                            Node
                                { start = { row = start.row, column = start.column - (index * 2) }
                                , end = { row = end.row, column = end.column - (index * 2) }
                                }
                                link
                        )
            )
        |> List.concat


linkParser : Int -> ModuleName -> Parser (Maybe (Node Link))
linkParser row moduleName =
    Parser.succeed identity
        |= Parser.getCol
        |. bracketsParser
        |> Parser.andThen
            (\col ->
                if col == 1 then
                    Parser.oneOf
                        [ inlineLinkParser
                            |> Parser.map Just
                        , referenceLinkParser
                            |> Parser.map Just
                        , Parser.succeed Nothing
                        ]

                else
                    Parser.oneOf
                        [ Parser.map Just inlineLinkParser
                        , Parser.succeed Nothing
                        ]
            )
        |> Parser.map
            (Maybe.map
                (\(Node range link) ->
                    Node (addOffset row range) (normalizeModuleName moduleName link)
                )
            )


normalizeModuleName : ModuleName -> Link -> Link
normalizeModuleName currentModuleName link =
    case link.file of
        ModuleTarget [] ->
            let
                file : FileTarget
                file =
                    if List.isEmpty currentModuleName then
                        ReadmeTarget

                    else
                        ModuleTarget currentModuleName
            in
            { link | file = file }

        ModuleTarget _ ->
            link

        ReadmeTarget ->
            link

        PackagesTarget _ ->
            link

        External _ ->
            link


{-| Parses things like:

    This is a [link](#Link).

-}
inlineLinkParser : Parser (Node Link)
inlineLinkParser =
    Parser.succeed
        (\( startRow, startCol ) link ( endRow, endCol ) ->
            Node
                { start = { row = startRow, column = startCol - 2 }
                , end = { row = endRow, column = endCol - 2 }
                }
                link
        )
        |. Parser.symbol "("
        |= Parser.getPosition
        |= pathParser ')'
        |= Parser.getPosition
        |. Parser.chompUntil ")"
        |. Parser.symbol ")"


{-| Parses things like:

    [link]: #Link

-}
referenceLinkParser : Parser (Node Link)
referenceLinkParser =
    Parser.succeed
        (\( startRow, startCol ) link ( endRow, endCol ) ->
            Node
                { start = { row = startRow, column = startCol - 2 }
                , end = { row = endRow, column = endCol - 2 }
                }
                link
        )
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.getPosition
        |= pathParser '\n'
        |= Parser.getPosition


pathParser : Char -> Parser Link
pathParser endChar =
    Parser.oneOf
        [ Parser.succeed
            (\section ->
                { file = ModuleTarget [], startsWith = StartsWithOther, slug = Just section }
            )
            |. Parser.symbol "#"
            |= idParser endChar
        , Parser.succeed
            (\startsWithDotSlash { fileTarget, startsWithSlash } slug ->
                { file = fileTarget
                , startsWith =
                    if startsWithDotSlash then
                        StartsWithDotSlash

                    else if startsWithSlash then
                        StartsWithSlash

                    else
                        StartsWithOther
                , slug = slug
                }
            )
            |= ignoreDotSlash
            |= parseModuleName
            |= optionalSectionParser endChar
        ]


optionalSectionParser : Char -> Parser (Maybe String)
optionalSectionParser endChar =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.symbol "#"
            |= idParser endChar
        , Parser.succeed Nothing
        ]


parseModuleName : Parser { fileTarget : FileTarget, startsWithSlash : Bool }
parseModuleName =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c /= '#' && c /= ')' && c /= ' ')
        |> Parser.getChompedString
        |> Parser.map
            (\linkTarget ->
                if linkTarget == "" then
                    { fileTarget = ReadmeTarget
                    , startsWithSlash = False
                    }

                else
                    case Parser.run onlyModuleNameParser linkTarget of
                        Ok moduleName ->
                            { fileTarget = ModuleTarget moduleName
                            , startsWithSlash = False
                            }

                        Err _ ->
                            case parseExternalPackageUrl linkTarget of
                                Just result ->
                                    result

                                Nothing ->
                                    { fileTarget = External linkTarget
                                    , startsWithSlash = False
                                    }
            )


parseExternalPackageUrl : String -> Maybe { fileTarget : FileTarget, startsWithSlash : Bool }
parseExternalPackageUrl urlString =
    let
        authorName =
            Url.Parser.s "packages"
                </> Url.Parser.string
                </> Url.Parser.string

        authorNameVersion =
            authorName
                </> Url.Parser.string

        authorNameVersionSection =
            authorNameVersion
                </> Url.Parser.string
                </> Url.Parser.fragment identity

        parser =
            Url.Parser.oneOf
                [ authorName
                    |> Url.Parser.map
                        (\author name ->
                            PackagesTarget
                                { name = author ++ "/" ++ name
                                , subTarget = VersionsSubTarget
                                }
                        )
                , authorNameVersion
                    |> Url.Parser.map
                        (\author name version ->
                            PackagesTarget
                                { name = author ++ "/" ++ name
                                , subTarget = ReadmeSubTarget version
                                }
                        )
                , authorNameVersionSection
                    |> Url.Parser.map
                        (\author name version module_ _ ->
                            PackagesTarget
                                { name = author ++ "/" ++ name
                                , subTarget =
                                    module_
                                        |> String.split "-"
                                        |> ModuleSubTarget version
                                }
                        )
                ]

        ( urlMaybe, startsWithSlash ) =
            case Url.fromString urlString of
                Just u ->
                    ( Just u
                    , False
                    )

                Nothing ->
                    ( Url.fromString ("https://package.elm-lang.org" ++ urlString)
                    , True
                    )
    in
    urlMaybe
        |> Maybe.andThen
            (\url ->
                if url.host == "package.elm-lang.org" then
                    Url.Parser.parse parser url
                        |> Maybe.map
                            (\fileTarget ->
                                { fileTarget = fileTarget
                                , startsWithSlash = startsWithSlash
                                }
                            )

                else
                    Nothing
            )


ignoreDotSlash : Parser Bool
ignoreDotSlash =
    Parser.oneOf
        [ Parser.symbol "."
            |. Parser.symbol "/"
            |> Parser.map (\_ -> True)
        , Parser.succeed False
        ]


bracketsParser : Parser ()
bracketsParser =
    Parser.succeed identity
        |. Parser.symbol "["
        |. Parser.spaces
        |= Parser.chompUntil "]"
        |. Parser.spaces
        |. Parser.symbol "]"


findParser : Parser a -> Parser (List a)
findParser parser =
    Parser.loop []
        (\parsed ->
            Parser.oneOf
                [ Parser.succeed (\p -> p :: parsed)
                    |= parser
                    |> Parser.map Parser.Loop
                , Parser.succeed parsed
                    |. Parser.chompIf (\_ -> True)
                    |> Parser.map Parser.Loop
                , Parser.end
                    |> Parser.map (\() -> Parser.Done (List.reverse parsed))
                ]
        )


{-| 0 or more things directly separated by a string like "go-gi-ga".
-}
manySeparated :
    { by : String
    , item : Parser between
    }
    -> Parser (List between)
manySeparated { by, item } =
    Parser.sequence
        { start = ""
        , separator = by
        , end = ""
        , spaces = Parser.symbol ""
        , item = item
        , trailing = Parser.Forbidden
        }
