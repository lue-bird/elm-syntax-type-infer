Add type information to the nodes
of an [elm-syntax](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree.

```elm
import Elm.Syntax.Node
import Elm.Syntax.Expression
import ElmSyntaxTypeInfer


[ { declaration =
        Elm.Syntax.Node.empty
            { expression =
                Elm.Syntax.Node.empty
                    (Elm.Syntax.Expression.ListExpr
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.Integer 1)
                        ]
                    )
            , name = Elm.Syntax.Node.empty "majorVersions"
            , arguments = []
            }
  , signature = Nothing
  , documentation = Nothing
  }
]
    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
        , moduleOriginLookup = exampleModuleOriginLookup
        , otherModuleDeclaredTypes =
            []
                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                    exampleModuleOriginLookup
                |> .types
        }
-->
Ok
    (FastDict.singleton "majorVersions"
        { type_ =
            ElmSyntaxTypeInfer.TypeNotVariable
                (ElmSyntaxTypeInfer.TypeConstruct
                    { moduleOrigin = [ "List" ]
                    , name = "List"
                    , arguments =
                        [ ElmSyntaxTypeInfer.TypeVariable "number" ]
                    }
                )
        ...
        }
    )


exampleModuleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookup =
    []
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes
```

## TODO

- right now, this can't handle medium to large files at an acceptable speed.
  Last measurement: for 10k lines, expect 0.8-2s  (report if you have other numbers)
- more tests

Optimization ideas
- go through typeUnify and add e.g. typeUnifyWithFunction
- idea: optimize for the case that all types do match (e.g. check for type equivalence, then shortcut)
- somehow avoid excessive Result.mapError in list substitutions combine
- when applying substitutions, check whether iteration via pop or toList etc is fastest and check for empty early specifically
- optimize equivalentVariableSetMerge, introduce typeMapVariableAndCollectResultingVariables
- direct lookup { signatures : FastDict.Dict (qualification,String) { moduleOrigin : ModuleName, type_ : Type }
, variants : ...
, typeConstructs : ...
, ...
 }
 