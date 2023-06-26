module Docs.Utils.LinkTest exposing (urlToExternalPackageReferenceTest)

import Docs.Utils.Link exposing (ExternalPackageReference(..), urlToExternalPackageReference)
import Expect
import Test exposing (Test, describe, test)


urlToExternalPackageReferenceTest : Test
urlToExternalPackageReferenceTest =
    let
        urlsWithAuthorAndName =
            [ "https://package.elm-lang.org/packages/elm/regex"
            , "https://package.elm-lang.org/packages/elm/regex/"
            , "/packages/elm/regex"
            , "/packages/elm/regex/"
            ]

        urlsWithAuthorNameAndVersion =
            [ "https://package.elm-lang.org/packages/elm/regex/1.1.1"
            , "https://package.elm-lang.org/packages/elm/regex/1.1.1/"
            , "/packages/elm/regex/1.1.1"
            , "/packages/elm/regex/1.1.1/"
            ]

        urlsWithAuthorNameVersionAndModule =
            [ "https://package.elm-lang.org/packages/elm/regex/1.1.1/Regex"
            , "/packages/elm/regex/1.1.1/Regex"
            ]

        urlsWithAuthorNameVersionModuleAndSection =
            [ "https://package.elm-lang.org/packages/elm/regex/1.1.1/Regex#replace"
            , "/packages/elm/regex/1.1.1/Regex#replace"
            ]
    in
    describe "urlToExternalPackageReference"
        [ describe "URLs with author and name"
            (urlsWithAuthorAndName
                |> List.map
                    (\url ->
                        test ("should parse: " ++ url) <|
                            \_ ->
                                urlToExternalPackageReference url
                                    |> Expect.equal
                                        (Just
                                            (ExternalPackageVersionSelectionReference
                                                { author = "elm"
                                                , name = "regex"
                                                }
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
                                urlToExternalPackageReference url
                                    |> Expect.equal
                                        (Just
                                            (ExternalPackageVersionReference
                                                { author = "elm"
                                                , name = "regex"
                                                , version = "1.1.1"
                                                }
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
                                urlToExternalPackageReference url
                                    |> Expect.equal
                                        (Just
                                            (ExternalPackageSectionReference
                                                { author = "elm"
                                                , name = "regex"
                                                , version = "1.1.1"
                                                , section = "Regex"
                                                }
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
                                urlToExternalPackageReference url
                                    |> Expect.equal
                                        (Just
                                            (ExternalPackageSectionReference
                                                { author = "elm"
                                                , name = "regex"
                                                , version = "1.1.1"
                                                , section = "Regex#replace"
                                                }
                                            )
                                        )
                    )
            )
        ]
