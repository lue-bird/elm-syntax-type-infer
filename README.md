Add type information to the nodes
of an [elm-syntax](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree.

> ⚠️ You should not rely on this for anything serious right now!
> It might still have bugs (e.g. it currently doesn't report let/module-level annotations that are too loose) and it's also relatively slow: for 10k lines, expect 0.1-1s depending on the amount of un-annotated declarations (report if you have other numbers)
>
> The purpose of publishing this early is for easy use in personal experiments
> and for especially interested folks to check API, documentation etc.


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
        { moduleName = "Sample"
        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
        , moduleOriginLookup = exampleModuleOriginLookup
        , otherModuleDeclaredTypes =
            []
                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                    exampleModuleOriginLookup
                |> .types
        }
-->
Ok
    [ { type_ =
        ElmSyntaxTypeInfer.TypeNotVariable
            (ElmSyntaxTypeInfer.TypeConstruct
                { moduleOrigin = [ "List" ]
                , name = "List"
                , arguments =
                    [ ElmSyntaxTypeInfer.TypeVariable
                        { name = "number", useRange = Elm.Syntax.Range.empty }
                    ]
                }
            )
      ...
      }
    ]


exampleModuleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookup =
    []
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes
```

If you're looking for how to use type inference across a whole project and its dependencies,
looking at [`elm-syntax-to-fsharp`](https://github.com/lue-bird/elm-syntax-to-fsharp) might help.
