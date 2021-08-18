module Docs.LinksPointToExistingPackageMembers exposing (rule)

import Elm.Module as Module
import Elm.Project as Project exposing (Project)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import EverySet exposing (EverySet)
import JaroWinkler
import ParserExtra as Parser
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import SyntaxHelp


rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.LinksPointToExistingPackageMembers" initialProjectContext
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = \_ _ -> fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation check
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { linksInReadme : Maybe (SourceAndLinks Rule.ReadmeKey)
    , linksInModules : List (SourceAndLinks Rule.ModuleKey)
    , exposed : EverySet SyntaxHelp.ModuleInfo
    }


type alias SourceAndLinks key =
    { key : key
    , links : EverySet LinkWithRange
    }


type alias LinkWithRange =
    { parsed : SyntaxHelp.Link
    , range : Range
    }


type alias ModuleContext =
    { docs : EverySet (Node String)
    , exposed : EverySet SyntaxHelp.ModuleInfo
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { exposed = EverySet.empty
    , linksInModules = []
    , linksInReadme = Nothing
    }


fromProjectToModule : ProjectContext -> ModuleContext
fromProjectToModule projectContext =
    { exposed = projectContext.exposed
    , docs = EverySet.empty
    }


fromModuleToProject : Rule.ModuleKey -> Node (List String) -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey (Node _ moduleName) { exposed, docs } =
    { linksInReadme = Nothing
    , exposed = exposed
    , linksInModules =
        [ { key = moduleKey
          , links =
                docs
                    |> EverySet.toList
                    |> List.concatMap
                        (\(Node { start } doc) ->
                            linksIn { doc = doc, start = start }
                        )
                    |> List.map (useIfNoModuleSpecified moduleName)
                    |> EverySet.fromList
          }
        ]
    }


useIfNoModuleSpecified :
    List String
    -> { range | parsed : SyntaxHelp.Link }
    -> { range | parsed : SyntaxHelp.Link }
useIfNoModuleSpecified moduleName ({ parsed } as match) =
    { match
        | parsed =
            case parsed.moduleName of
                [] ->
                    { parsed | moduleName = moduleName }

                _ ->
                    match.parsed
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposed = EverySet.union newContext.exposed previousContext.exposed
    , linksInModules = List.append newContext.linksInModules previousContext.linksInModules
    , linksInReadme = previousContext.linksInReadme
    }



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List nothing, ProjectContext )
readmeVisitor maybeReadme context =
    ( []
    , case maybeReadme of
        Just readme ->
            { context | linksInReadme = Just (findLinksInReadme readme) }

        Nothing ->
            context
    )



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { key_ | project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson context =
    ( []
    , { context
        | exposed =
            maybeElmJson
                |> Maybe.map exposedModulesInElmJson
                |> Maybe.withDefault EverySet.empty
      }
    )


exposedModulesInElmJson : { key_ | project : Project } -> EverySet SyntaxHelp.ModuleInfo
exposedModulesInElmJson { project } =
    case project of
        Project.Package { exposed } ->
            SyntaxHelp.exposedModules exposed
                |> EverySet.fromList
                |> EverySet.map
                    (\name ->
                        { moduleName =
                            name |> Module.toString |> String.split "."
                        , exposedDefinitions = ( SyntaxHelp.Explicit, [] )
                        }
                    )

        Project.Application _ ->
            EverySet.empty


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor exposedInModule
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withCommentsVisitor commentsVisitor


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor (Node _ declaration) context =
    ( []
    , case SyntaxHelp.docOfDeclaration declaration of
        Just doc ->
            insertDoc context doc

        Nothing ->
            context
    )


commentsVisitor : List (Node String) -> ModuleContext -> ( List nothing, ModuleContext )
commentsVisitor comments context =
    ( []
    , comments
        |> find (Node.value >> SyntaxHelp.isFileComment)
        |> Maybe.map (insertDoc context)
        |> Maybe.withDefault context
    )


insertDoc : ModuleContext -> Node String -> ModuleContext
insertDoc context doc =
    { context | docs = EverySet.insert doc context.docs }


linksIn :
    { doc : String, start : Location }
    -> List LinkWithRange
linksIn { doc, start } =
    doc
        |> Parser.find SyntaxHelp.linkParser
        |> List.map
            (\match ->
                { match
                    | range =
                        { start = SyntaxHelp.addLocation start match.range.start
                        , end = SyntaxHelp.addLocation start match.range.end
                        }
                }
            )


findLinksInReadme : { readmeKey : Rule.ReadmeKey, content : String } -> SourceAndLinks Rule.ReadmeKey
findLinksInReadme readme =
    let
        { readmeKey, content } =
            readme
    in
    { key = readmeKey
    , links =
        linksIn
            { doc = content
            , start = { row = 1, column = 1 }
            }
            |> EverySet.fromList
    }


exposedInModule :
    Node Module
    -> ModuleContext
    -> ( List error_, ModuleContext )
exposedInModule (Node _ module_) context =
    ( []
    , { context
        | exposed =
            let
                info : SyntaxHelp.ModuleInfo
                info =
                    SyntaxHelp.moduleInfo module_
            in
            if
                context.exposed
                    |> EverySet.toList
                    |> List.any
                        (.moduleName
                            >> (==) info.moduleName
                        )
            then
                EverySet.insert info context.exposed

            else
                context.exposed
      }
    )


check : ProjectContext -> List (Rule.Error scope)
check { linksInReadme, exposed, linksInModules } =
    let
        exposedMembers : Set String
        exposedMembers =
            exposed
                |> EverySet.toList
                |> List.concatMap
                    (\{ moduleName, exposedDefinitions } ->
                        let
                            moduleNameString : String
                            moduleNameString =
                                String.join "." moduleName

                            ( _, exposedDefs ) =
                                exposedDefinitions
                        in
                        moduleNameString
                            :: List.map
                                (\def -> moduleNameString ++ "." ++ def)
                                exposedDefs
                    )
                |> Set.fromList

        errorsForLinksInReadme : List (Rule.Error scope)
        errorsForLinksInReadme =
            linksInReadme
                |> Maybe.map (errorForLinkInReadme exposed exposedMembers)
                |> Maybe.withDefault []

        errorsForLinksInModules : List (Rule.Error scope)
        errorsForLinksInModules =
            List.concatMap (errorForLinkInModule exposed exposedMembers) linksInModules
    in
    List.append errorsForLinksInReadme errorsForLinksInModules


errorForLinkInReadme : EverySet SyntaxHelp.ModuleInfo -> Set String -> SourceAndLinks Rule.ReadmeKey -> List (Rule.Error scope)
errorForLinkInReadme exposed exposedMembers { key, links } =
    links
        |> EverySet.toList
        |> List.concatMap
            (\match ->
                case ( match.parsed.moduleName, match.parsed.kind ) of
                    ( [], SyntaxHelp.DefinitionLink definition ) ->
                        [ Rule.errorForReadme key
                            (noModuleSpecifiedForDefinitionInLinkInReadme
                                { badLink = definition
                                , exposed = Set.toList exposedMembers
                                }
                            )
                            match.range
                        ]

                    _ ->
                        checkLink exposed exposedMembers (Rule.errorForReadme key) match
            )


errorForLinkInModule : EverySet SyntaxHelp.ModuleInfo -> Set String -> SourceAndLinks Rule.ModuleKey -> List (Rule.Error scope)
errorForLinkInModule exposed exposedMembers { key, links } =
    links
        |> EverySet.toList
        |> List.concatMap (checkLink exposed exposedMembers (Rule.errorForModule key))


checkLink :
    EverySet SyntaxHelp.ModuleInfo
    -> Set String
    -> ({ message : String, details : List String } -> Range -> Rule.Error scope)
    -> LinkWithRange
    -> List (Rule.Error scope)
checkLink exposed exposedMembers error match =
    let
        { moduleName, kind } =
            match.parsed
    in
    case kind of
        SyntaxHelp.ModuleLink ->
            if
                exposed
                    |> EverySet.toList
                    |> List.any
                        (.moduleName
                            >> (==) moduleName
                        )
            then
                []

            else
                [ error
                    { message = moduleInLinkNotExposed
                    , details = details exposedMembers moduleName
                    }
                    match.range
                ]

        SyntaxHelp.DefinitionLink definition ->
            if
                exposed
                    |> EverySet.toList
                    |> List.any
                        (\m ->
                            (m.moduleName == moduleName)
                                && (m.exposedDefinitions
                                        |> SyntaxHelp.isExposed definition
                                   )
                        )
            then
                []

            else
                [ error
                    { message = definitionInLinkNotExposedMessage
                    , details = details exposedMembers moduleName
                    }
                    match.range
                ]


details : Set String -> ModuleName -> List String
details exposedMembers moduleNameParts =
    linkPointsToNonExistentMemberDetails
        { exposed = Set.toList exposedMembers
        , badLink = String.join "." moduleNameParts
        }


definitionInLinkNotExposedMessage : String
definitionInLinkNotExposedMessage =
    "The link points to a definition that isn't exposed from any exposed module in this package."


moduleInLinkNotExposed : String
moduleInLinkNotExposed =
    "The link points to a module that isn't listed in \"exposed-modules\"."


noModuleSpecifiedForDefinitionInLinkInReadme :
    { exposed : List String, badLink : String }
    -> { message : String, details : List String }
noModuleSpecifiedForDefinitionInLinkInReadme { exposed, badLink } =
    let
        moduleSuggestions : List String
        moduleSuggestions =
            exposed
                |> List.filter (String.endsWith badLink)
                |> List.map (String.dropRight (String.length badLink))
    in
    { message = "Using a link of the form [..](#definition) in the readme."
    , details =
        [ "There's no way to figure out in which module to look for this definition."
        , "I found `" ++ badLink ++ "` in the module(s) " ++ String.join ", " moduleSuggestions ++ "."
        ]
    }


linkPointsToNonExistentMemberDetails : { exposed : List String, badLink : String } -> List String
linkPointsToNonExistentMemberDetails { exposed, badLink } =
    let
        suggestions : String
        suggestions =
            exposed
                |> List.sortBy
                    (\member ->
                        -(JaroWinkler.similarity badLink member)
                    )
                |> List.take 3
                |> String.join ", "
    in
    [ "Links are only useful when they point to exposed package members."
    , "Maybe you meant one of those: " ++ suggestions ++ "."
    ]



-- Utils


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        head :: rest ->
            if predicate head then
                Just head

            else
                find predicate rest
