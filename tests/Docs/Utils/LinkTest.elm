module Docs.Utils.LinkTest exposing (parseExternalPackageUrlTest)

import Docs.Utils.Link exposing (FileTarget(..), LinkStartsWith(..), SubTarget(..), parseExternalPackageUrl)
import Expect
import Test exposing (Test, describe, test)


parseExternalPackageUrlTest : Test
parseExternalPackageUrlTest =
    let
        urlsWithAuthorAndName =
            [ "https://package.elm-lang.org/packages/elm/regex"
            , "https://package.elm-lang.org/packages/elm/regex/"
            ]

        absolutePathUrlsWithAuthorAndName =
            [ "/packages/elm/regex"
            , "/packages/elm/regex/"
            ]

        urlsWithAuthorNameAndVersion =
            [ "https://package.elm-lang.org/packages/elm/regex/1.1.1"
            , "https://package.elm-lang.org/packages/elm/regex/1.1.1/"
            ]

        absolutePathUrlsWithAuthorNameAndVersion =
            [ "/packages/elm/regex/1.1.1"
            , "/packages/elm/regex/1.1.1/"
            ]

        urlsWithAuthorNameVersionAndModule =
            [ "https://package.elm-lang.org/packages/elm/regex/1.1.1/Regex"
            ]

        absolutePathUrlsWithAuthorNameVersionAndModule =
            [ "/packages/elm/regex/1.1.1/Regex"
            ]

        urlsWithAuthorNameVersionModuleAndSection =
            [ "https://package.elm-lang.org/packages/elm/regex/1.1.1/Regex#replace"
            ]

        absolutePathUrlsWithAuthorNameVersionModuleAndSection =
            [ "/packages/elm/regex/1.1.1/Regex#replace"
            ]

        invalidPackageUrls =
            [ "https://www.example.com/packages/elm/regex"
            , "https://www.example.com/packages/elm/regex/1.1.1/"
            , "https://www.example.com/packages/elm/regex/1.1.1/Regex"
            , "https://www.example.com/packages/elm/regex/1.1.1/Regex#replace"
            , "https://package.elm-lang.org/wrong/elm/regex"
            , "https://package.elm-lang.org/wrong/elm/regex/1.1.1/"
            , "https://package.elm-lang.org/wrong/elm/regex/1.1.1/Regex"
            , "https://package.elm-lang.org/wrong/elm/regex/1.1.1/Regex#replace"
            , "https://package.elm-lang.org/packages/elm/"
            , "ftp://package.elm-lang.org/packages/elm/regex/1.1.1/Regex#replace"
            , "/wrong/elm/regex"
            , "/wrong/elm/regex/1.1.1/"
            , "/wrong/elm/regex/1.1.1/Regex"
            , "/wrong/elm/regex/1.1.1/Regex#replace"
            , "/packages/elm/"
            ]
    in
    describe "parseExternalPackageUrl"
        [ describe "URLs with author and name"
            (urlsWithAuthorAndName
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal
                                        (Just
                                            ( PackagesTarget
                                                { name = "elm/regex"
                                                , subTarget = VersionsSubTarget
                                                }
                                            , OtherLinkStart
                                            )
                                        )
                    )
            )
        , describe "Absolute-path URLs with author and name"
            (absolutePathUrlsWithAuthorAndName
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal
                                        (Just
                                            ( PackagesTarget
                                                { name = "elm/regex"
                                                , subTarget = VersionsSubTarget
                                                }
                                            , LinkStartsWithSlash
                                            )
                                        )
                    )
            )
        , describe "URLs with author, name and version"
            (urlsWithAuthorNameAndVersion
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal
                                        (Just
                                            ( PackagesTarget
                                                { name = "elm/regex"
                                                , subTarget = ReadmeSubTarget "1.1.1"
                                                }
                                            , OtherLinkStart
                                            )
                                        )
                    )
            )
        , describe "Absolute-path URLs with author, name and version"
            (absolutePathUrlsWithAuthorNameAndVersion
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal
                                        (Just
                                            ( PackagesTarget
                                                { name = "elm/regex"
                                                , subTarget = ReadmeSubTarget "1.1.1"
                                                }
                                            , LinkStartsWithSlash
                                            )
                                        )
                    )
            )
        , describe "URLs with author, name, version and section"
            (urlsWithAuthorNameVersionAndModule
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal
                                        (Just
                                            ( PackagesTarget
                                                { name = "elm/regex"
                                                , subTarget = ModuleSubTarget "1.1.1" [ "Regex" ]
                                                }
                                            , OtherLinkStart
                                            )
                                        )
                    )
            )
        , describe "Absolute-path URLs with author, name, version and section"
            (absolutePathUrlsWithAuthorNameVersionAndModule
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal
                                        (Just
                                            ( PackagesTarget
                                                { name = "elm/regex"
                                                , subTarget = ModuleSubTarget "1.1.1" [ "Regex" ]
                                                }
                                            , LinkStartsWithSlash
                                            )
                                        )
                    )
            )
        , describe "URLs with author, name, version, module and section"
            (urlsWithAuthorNameVersionModuleAndSection
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal
                                        (Just
                                            ( PackagesTarget
                                                { name = "elm/regex"
                                                , subTarget = ModuleSubTarget "1.1.1" [ "Regex" ]
                                                }
                                            , OtherLinkStart
                                            )
                                        )
                    )
            )
        , describe "Absolute-path URLs with author, name, version, module and section"
            (absolutePathUrlsWithAuthorNameVersionModuleAndSection
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal
                                        (Just
                                            ( PackagesTarget
                                                { name = "elm/regex"
                                                , subTarget = ModuleSubTarget "1.1.1" [ "Regex" ]
                                                }
                                            , LinkStartsWithSlash
                                            )
                                        )
                    )
            )
        , describe "Invalid package URLs"
            (invalidPackageUrls
                |> List.map
                    (\url ->
                        test ("should fail to parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageUrl url
                                    |> Expect.equal Nothing
                    )
            )
        ]
