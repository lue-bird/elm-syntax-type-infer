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

-   add more tests
-   never substitute partial variable uses (maybe by instead tracking partials as only the variable name necessary to search for instances)
-   (mutually) recursive type aliases can run into an infinite loop
-   add local types in a fully separate (optional!) step

### performance problems?

Right now, this can't handle medium to large files at an acceptable speed.
