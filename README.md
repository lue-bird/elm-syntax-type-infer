Add type information to the nodes
of an [elm-syntax](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree.

```elm
import Elm.Syntax.Node
import Elm.Syntax.Expression
import ElmSyntaxTypeInfer


{ declaration =
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
    |> ElmSyntaxTypeInfer.declarationExpression
        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
        , moduleOriginLookup = exampleModuleOriginLookup
        , otherModuleDeclaredTypes =
            []
                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                    exampleModuleOriginLookup
        }
    |> Result.map
        (\typedDeclaration ->
            typedDeclaration.declaration
                |> Elm.Syntax.Node.value
                |> .expression
                |> .type_
        )
-->
Ok
    (ElmSyntaxTypeInfer.TypeConstruct
        { moduleOrigin = [ "List" ]
        , name = "List"
        , arguments =
            [  ElmSyntaxTypeInfer.TypeNumberVariable
                ( [ "0", "implementation" ], "number" )
            ]
        }
    )


exampleModuleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookup =
    []
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes
```

## TODO

-   finish current implementation (TODO comments)
-   is unification between in and output types always the same? Especially with regards to type variable constraints
-   are variable constraints upheld in all places (equivalence for example?)
-   (mutually) recursive type aliases can run into an infinite loop
-   mutually recursive types (e.g. substituting `a -> { x : b }` and `b -> { x : a }`) can run into an infinite loop
-   prefer variable names without context to those with context when creating a variable name for equivalent variables
    (to keep let declared type variables)

### performance problems?

Right now, this can't handle medium to large files at an acceptable speed.
