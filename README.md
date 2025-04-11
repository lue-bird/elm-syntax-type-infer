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

-   add local types in a fully separate (optional!) step
-   add more tests
-   optimize substitution scope similar to how patterns are currently inferred:
    Substitutions should be applied as they are created except for partials and local variables
-   (mutually) recursive type aliases can run into an infinite loop

### performance problems?

Right now, this can't handle medium to large files at an acceptable speed.
