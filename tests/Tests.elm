module Tests exposing (suite)

import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Node
import ElmSyntaxTypeInfer
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "ElmSyntaxTypeInfer"
        [ Test.test "readme example"
            (\() ->
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
                    |> ElmSyntaxTypeInfer.expressionDeclaration
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookup
                                |> .types
                        }
                    |> Result.map (\typed -> typed.result.type_)
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "List" ]
                                    , name = "List"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeVariable
                                            ( [ "0", "result" ], "number" )
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "unify integer and float in list"
            (\() ->
                Elm.Syntax.Expression.ListExpr
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Integer 1)
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 2.2)
                    ]
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "List" ]
                                , name = "List"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                        )
            )
        , Test.test "unify integer and float in addition"
            (\() ->
                Elm.Syntax.Expression.OperatorApplication
                    "+"
                    Elm.Syntax.Infix.Left
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Integer 1)
                    )
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 2.2)
                    )
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Basics" ]
                                , name = "Float"
                                , arguments = []
                                }
                            )
                        )
            )
        , Test.test "(independent) integers and float in triple"
            (\() ->
                Elm.Syntax.Expression.TupledExpression
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Integer 1)
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 2.2)
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Integer 3)
                    ]
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeTriple
                                { part0 =
                                    ElmSyntaxTypeInfer.TypeVariable
                                        ( [ "0", "result" ], "number" )
                                , part1 =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , part2 =
                                    ElmSyntaxTypeInfer.TypeVariable
                                        ( [ "2", "result" ], "number" )
                                }
                            )
                        )
            )
        , Test.test "identity called with float (from implicit import)"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [] "identity")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 2.2)
                    ]
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Basics" ]
                                , name = "Float"
                                , arguments = []
                                }
                            )
                        )
            )
        , Test.test "identity called with integer (from implicit import)"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [] "identity")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Integer 1)
                    ]
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeVariable
                            ( [ "a", "called", "result", "_and", "number", "argument0", "result", "_and", "result", "result" ]
                            , "numberEquivalent"
                            )
                        )
            )
        , Test.test "Basics.identity called with float (qualified from implicit import)"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Basics" ] "identity")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 2.2)
                    ]
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Basics" ]
                                , name = "Float"
                                , arguments = []
                                }
                            )
                        )
            )
        , Test.test "Basics.identity <| float (qualified from implicit import)"
            (\() ->
                Elm.Syntax.Expression.OperatorApplication
                    "<|"
                    Elm.Syntax.Infix.Right
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Basics" ] "identity")
                    )
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 2.2)
                    )
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Basics" ]
                                , name = "Float"
                                , arguments = []
                                }
                            )
                        )
            )
        , Test.test "float |> Basics.identity (qualified from implicit import)"
            (\() ->
                Elm.Syntax.Expression.OperatorApplication
                    "|>"
                    Elm.Syntax.Infix.Left
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 2.2)
                    )
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Basics" ] "identity")
                    )
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Basics" ]
                                , name = "Float"
                                , arguments = []
                                }
                            )
                        )
            )
        ]


expressionExpectInferredType :
    ElmSyntaxTypeInfer.Type ElmSyntaxTypeInfer.TypeVariableFromContext
    -> Elm.Syntax.Expression.Expression
    -> Expect.Expectation
expressionExpectInferredType expectedInferredType expression =
    { declaration =
        Elm.Syntax.Node.empty
            { expression =
                Elm.Syntax.Node.empty expression
            , name = Elm.Syntax.Node.empty "majorVersions"
            , arguments = []
            }
    , signature = Nothing
    , documentation = Nothing
    }
        |> ElmSyntaxTypeInfer.expressionDeclaration
            { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
            , moduleOriginLookup = exampleModuleOriginLookup
            , otherModuleDeclaredTypes =
                []
                    |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                        exampleModuleOriginLookup
                    |> .types
            }
        |> Result.map (\typed -> typed.result.type_)
        |> Expect.equal (Ok expectedInferredType)


exampleModuleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookup =
    []
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes
