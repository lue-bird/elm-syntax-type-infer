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
                ( [ "0", "result" ], "number" )
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
-   (mutually) recursive type aliases can run into an infinite loop
-   mutually recursive types (e.g. substituting `a -> { x : b }` and `b -> { x : a }`) can run into an infinite loop
-   somehow "elevate" let declaration types to "module declaration level".
    Do _not_ let usage influence their types
    (forall type variables).
    To infer (mutually) recursive let declarations:
    substituting in the type of a (let) declaration also substitutes its "descendants"
    but not the other way around
-   type infer record type alias constructor function reference expression.
    Requires storing `Maybe (List String)` field order in module type alias declaration types

### performance problems?

Right now, this can't handle medium to large files at an acceptable speed.
