module Docs.Utils.LinkTest exposing (parseExternalPackageLinkTest)

import Docs.Utils.Link exposing (FileTarget(..), SubTarget(..), parseExternalPackageLink)
import Expect
import Test exposing (Test, describe, test)


parseExternalPackageLinkTest : Test
parseExternalPackageLinkTest =
    describe "parseExternalPackageLink"
        [ describe "URLs with author and name"
            ([ "https://package.elm-lang.org/packages/elm/regex"
             , "https://package.elm-lang.org/packages/elm/regex/"
             ]
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal
                                        (Just
                                            { fileTarget =
                                                PackagesTarget
                                                    { name = "elm/regex"
                                                    , subTarget = VersionsSubTarget
                                                    }
                                            , startsWithSlash = False
                                            }
                                        )
                    )
            )
        , describe "Absolute-path URLs with author and name"
            ([ "/packages/elm/regex"
             , "/packages/elm/regex/"
             ]
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal
                                        (Just
                                            { fileTarget =
                                                PackagesTarget
                                                    { name = "elm/regex"
                                                    , subTarget = VersionsSubTarget
                                                    }
                                            , startsWithSlash = True
                                            }
                                        )
                    )
            )
        , describe "URLs with author, name and version"
            ([ "https://package.elm-lang.org/packages/elm/regex/1.1.1"
             , "https://package.elm-lang.org/packages/elm/regex/1.1.1/"
             ]
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal
                                        (Just
                                            { fileTarget =
                                                PackagesTarget
                                                    { name = "elm/regex"
                                                    , subTarget = ReadmeSubTarget "1.1.1"
                                                    }
                                            , startsWithSlash = False
                                            }
                                        )
                    )
            )
        , describe "Absolute-path URLs with author, name and version"
            ([ "/packages/elm/regex/1.1.1"
             , "/packages/elm/regex/1.1.1/"
             ]
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal
                                        (Just
                                            { fileTarget =
                                                PackagesTarget
                                                    { name = "elm/regex"
                                                    , subTarget = ReadmeSubTarget "1.1.1"
                                                    }
                                            , startsWithSlash = True
                                            }
                                        )
                    )
            )
        , describe "URLs with author, name, version and module"
            ([ "https://package.elm-lang.org/packages/elm/regex/1.1.1/Regex"
             ]
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal
                                        (Just
                                            { fileTarget =
                                                PackagesTarget
                                                    { name = "elm/regex"
                                                    , subTarget = ModuleSubTarget "1.1.1" [ "Regex" ]
                                                    }
                                            , startsWithSlash = False
                                            }
                                        )
                    )
            )
        , describe "Absolute-path URLs with author, name, version and module"
            ([ "/packages/elm/regex/1.1.1/Regex"
             ]
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal
                                        (Just
                                            { fileTarget =
                                                PackagesTarget
                                                    { name = "elm/regex"
                                                    , subTarget = ModuleSubTarget "1.1.1" [ "Regex" ]
                                                    }
                                            , startsWithSlash = True
                                            }
                                        )
                    )
            )
        , describe "URLs with author, name, version, module and section"
            ([ "https://package.elm-lang.org/packages/elm/regex/1.1.1/Regex#replace"
             ]
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal
                                        (Just
                                            { fileTarget =
                                                PackagesTarget
                                                    { name = "elm/regex"
                                                    , subTarget = ModuleSubTarget "1.1.1" [ "Regex" ]
                                                    }
                                            , startsWithSlash = False
                                            }
                                        )
                    )
            )
        , describe "Absolute-path URLs with author, name, version, module and section"
            ([ "/packages/elm/regex/1.1.1/Regex#replace"
             ]
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal
                                        (Just
                                            { fileTarget =
                                                PackagesTarget
                                                    { name = "elm/regex"
                                                    , subTarget = ModuleSubTarget "1.1.1" [ "Regex" ]
                                                    }
                                            , startsWithSlash = True
                                            }
                                        )
                    )
            )
        , describe "Invalid package URLs"
            ([ "https://www.example.com/packages/elm/regex"
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
                |> List.map
                    (\url ->
                        test ("should fail to parse: " ++ url) <|
                            \_ ->
                                parseExternalPackageLink url
                                    |> Expect.equal Nothing
                    )
            )
        ]
