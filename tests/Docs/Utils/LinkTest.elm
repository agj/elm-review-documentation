module Docs.Utils.LinkTest exposing (urlToExternalPackageReferenceTest)

import Docs.Utils.Link exposing (urlToExternalPackageReference)
import Expect
import Test exposing (Test, describe, test)


urlToExternalPackageReferenceTest : Test
urlToExternalPackageReferenceTest =
    let
        urlsWithAuthorAndName =
            [ "/packages/elm/regex"
            , "/packages/elm/regex/"
            ]

        urlsWithAuthorNameAndVersion =
            [ "/packages/elm/regex/latest"
            , "/packages/elm/regex/latest/"
            , "/packages/elm/regex/1.1.1"
            , "/packages/elm/regex/1.1.1/"
            ]

        other =
            [ "/packages/elm/regex/latest/Regex"
            , "/packages/elm/regex/latest/Regex#Regex"
            , "/packages/elm/regex/latest/Regex#replace"
            , "/packages/mdgriffith/elm-ui/latest/Element-Background#image"
            , "/packages/mdgriffith/elm-ui/1.1.7/Element-Background"
            , "/packages/mdgriffith/elm-ui"
            ]
    in
    describe "urlToExternalPackageReference"
        (urlsWithAuthorAndName
            ++ urlsWithAuthorNameAndVersion
            ++ other
            |> List.map
                (\url ->
                    test ("should parse absolute-path URL: " ++ url) <|
                        \_ ->
                            urlToExternalPackageReference url
                                |> Expect.notEqual Nothing
                )
        )
