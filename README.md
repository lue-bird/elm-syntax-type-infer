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
    |> ElmSyntaxTypeInfer.valueOrFunctionDeclarations
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

-   add local types in a fully separate (optional!) step
-   add more tests
-   (mutually) recursive type aliases can run into an infinite loop
-   type infer record type alias constructor function reference expression
-   optimize equivalentVariablesMergeWithSetOf2, potentially de-optimize typedNodeReplaceTypeBy

### performance problems?

Right now, this can't handle medium to large files at an acceptable speed.
