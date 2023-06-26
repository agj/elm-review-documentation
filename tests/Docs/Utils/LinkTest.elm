module Docs.Utils.LinkTest exposing (packageLinkParserTest)

import Docs.Utils.Link exposing (packageLinkParser)
import Expect
import Parser exposing (Parser)
import Test exposing (Test, describe, test)


packageLinkParserTest : Test
packageLinkParserTest =
    let
        validUrls =
            [ "/packages/elm/regex/latest/Regex"
            , "/packages/elm/regex/latest/Regex#Regex"
            , "/packages/elm/regex/latest/Regex#replace"
            , "/packages/mdgriffith/elm-ui/latest/Element-Background#image"
            , "/packages/mdgriffith/elm-ui/1.1.7/Element-Background"
            , "/packages/mdgriffith/elm-ui"
            ]
    in
    describe "packageLinkParser"
        (validUrls
            |> List.map
                (\url ->
                    test ("should parse absolute-path URL: " ++ url) <|
                        \_ ->
                            Parser.run packageLinkParser url
                                |> isOk
                                |> Expect.equal True
                )
        )



-- UTILS


isOk : Result x a -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
