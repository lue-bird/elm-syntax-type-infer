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
        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypeContext
        , moduleOriginLookup =
            []
                |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                    ElmSyntaxTypeInfer.elmCoreTypeContext
        , moduleDeclaredTypes =
            { signatures = FastDict.empty
            , typeAliases = FastDict.empty
            , choiceTypes = FastDict.empty
            }
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
    (ElmSyntaxTypeInfer.TypeList
        (ElmSyntaxTypeInfer.TypeNumberVariable "number")
    )
```

## TODO

-   finish current implementation (TODO comments)
-   is unification between in and output types always the same? Especially with regards to type variable constriaints
-   are variable constraints upheld in all places (equivalence for example?)
-   (mutually) recursive type aliases can run into an infinite loop
-   mutually recursive types (e.g. substituting `a -> { x : b }` and `b -> { x : a }`) can run into an infinite loop

### performance problems?

Right now, this can't handle medium to large files at an acceptable speed.
