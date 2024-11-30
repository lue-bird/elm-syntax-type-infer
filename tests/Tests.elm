module Tests exposing (suite)

import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import ElmSyntaxTypeInfer
import Expect
import FastDict
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
                    |> Result.map .type_
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "List" ]
                                    , name = "List"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeVariable
                                            ( [ "0", "declarationResult" ], "number" )
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
                                        ( [ "0", "declarationResult" ], "number" )
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
                                        ( [ "2", "declarationResult" ], "number" )
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
                            ( [ "a", "called", "declarationResult", "_and", "callResult", "declarationResult", "_and", "declarationResult", "_and", "number", "argument0", "declarationResult" ]
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
        , Test.test "List.map called with Basics.identity (qualified from implicit import)"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Basics" ] "identity")
                    ]
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    ( [ "a", "argument0", "declarationResult", "_and", "a", "called", "declarationResult", "_and", "b", "called", "declarationResult" ]
                                                    , "equivalent"
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    ( [ "a", "argument0", "declarationResult", "_and", "a", "called", "declarationResult", "_and", "b", "called", "declarationResult" ]
                                                    , "equivalent"
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "let (a) = 2.2 in a"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetDestructuring
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.ParenthesizedPattern
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Pattern.VarPattern "a")
                                        )
                                    )
                                )
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Floatable 2.2)
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                    }
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
        , Test.test "case 2.2 of (a) -> a"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.ParenthesizedPattern
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.VarPattern "a")
                                    )
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.FunctionOrValue [] "a")
                          )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.Floatable 2.2)
                    }
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
        , Test.test "ignored argument pattern variable \\(a) -> [ 1, 2.2 ]"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.ParenthesizedPattern
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "a")
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Integer 1)
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Floatable 2.2)
                                ]
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeVariable ( [ "parameter0", "declarationResult" ], "a" )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { arguments = []
                                                        , moduleOrigin = [ "Basics" ]
                                                        , name = "Float"
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "argument pattern variable nuified with Float \\(a) -> [ a, 2.2 ]"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.ParenthesizedPattern
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "a")
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Floatable 2.2)
                                ]
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { arguments = []
                                            , moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { arguments = []
                                                        , moduleOrigin = [ "Basics" ]
                                                        , name = "Float"
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "argument pattern variable negated parenthesized \\(a) -> -(a)"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.ParenthesizedPattern
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "a")
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.Negation
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                        )
                                    )
                                )
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeVariable
                                        ( [ "a", "parameter0", "declarationResult", "_and", "numberNegated", "lambdaResult", "declarationResult" ]
                                        , "numberEquivalent"
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeVariable
                                        ( [ "a", "parameter0", "declarationResult", "_and", "numberNegated", "lambdaResult", "declarationResult" ]
                                        , "numberEquivalent"
                                        )
                                }
                            )
                        )
            )
        , Test.test "argument pattern variable called in negate parenthesized, implicit import \\(a) -> Basics.negate (a)"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.ParenthesizedPattern
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "a")
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "negate")
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                        )
                                    )
                                ]
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeVariable
                                        ( [ "a", "parameter0", "declarationResult", "_and", "callResult", "lambdaResult", "declarationResult", "_and", "number", "called", "lambdaResult", "declarationResult" ]
                                        , "numberEquivalent"
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeVariable
                                        ( [ "a", "parameter0", "declarationResult", "_and", "callResult", "lambdaResult", "declarationResult", "_and", "number", "called", "lambdaResult", "declarationResult" ]
                                        , "numberEquivalent"
                                        )
                                }
                            )
                        )
            )
        , Test.test "argument pattern variable equivalent to number variable \\(a) -> [ a, 1 ]"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.ParenthesizedPattern
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "a")
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Integer 1)
                                ]
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeVariable
                                        ( [ "a", "parameter0", "declarationResult", "_and", "number", "1", "lambdaResult", "declarationResult" ]
                                        , "numberEquivalent"
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    ( [ "a", "parameter0", "declarationResult", "_and", "number", "1", "lambdaResult", "declarationResult" ]
                                                    , "numberEquivalent"
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "record update union with same record veriable \\(a) -> [ { a | a = (), b = 1 }, { a | c = (), b = 2.2 } ]"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.ParenthesizedPattern
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "a")
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.RecordUpdateExpression
                                        (Elm.Syntax.Node.empty "a")
                                        [ Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "a"
                                            , Elm.Syntax.Node.empty
                                                Elm.Syntax.Expression.UnitExpr
                                            )
                                        , Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "b"
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Integer 1)
                                            )
                                        ]
                                    )
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.RecordUpdateExpression
                                        (Elm.Syntax.Node.empty "a")
                                        [ Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "c"
                                            , Elm.Syntax.Node.empty
                                                Elm.Syntax.Expression.UnitExpr
                                            )
                                        , Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "b"
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Floatable 2.2)
                                            )
                                        ]
                                    )
                                ]
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeRecordExtension
                                            { recordVariable =
                                                ( [ "_of", "record", "0", "lambdaResult", "declarationResult", "_and", "record", "1", "lambdaResult", "declarationResult" ]
                                                , "base"
                                                )
                                            , fields =
                                                FastDict.fromList
                                                    [ ( "a"
                                                      , ElmSyntaxTypeInfer.TypeNotVariable
                                                            ElmSyntaxTypeInfer.TypeUnit
                                                      )
                                                    , ( "c"
                                                      , ElmSyntaxTypeInfer.TypeNotVariable
                                                            ElmSyntaxTypeInfer.TypeUnit
                                                      )
                                                    , ( "b"
                                                      , ElmSyntaxTypeInfer.TypeNotVariable
                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                { arguments = []
                                                                , moduleOrigin = [ "Basics" ]
                                                                , name = "Float"
                                                                }
                                                            )
                                                      )
                                                    ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeRecordExtension
                                                        { recordVariable =
                                                            ( [ "_of", "record", "0", "lambdaResult", "declarationResult", "_and", "record", "1", "lambdaResult", "declarationResult" ]
                                                            , "base"
                                                            )
                                                        , fields =
                                                            FastDict.fromList
                                                                [ ( "a"
                                                                  , ElmSyntaxTypeInfer.TypeNotVariable
                                                                        ElmSyntaxTypeInfer.TypeUnit
                                                                  )
                                                                , ( "c"
                                                                  , ElmSyntaxTypeInfer.TypeNotVariable
                                                                        ElmSyntaxTypeInfer.TypeUnit
                                                                  )
                                                                , ( "b"
                                                                  , ElmSyntaxTypeInfer.TypeNotVariable
                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                            { arguments = []
                                                                            , moduleOrigin = [ "Basics" ]
                                                                            , name = "Float"
                                                                            }
                                                                        )
                                                                  )
                                                                ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "record update union with record \\a -> [ { a | b = 1 }, { c = (), b = 2.2 } ]"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "a")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.RecordUpdateExpression
                                        (Elm.Syntax.Node.empty "a")
                                        [ Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "b"
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Integer 1)
                                            )
                                        ]
                                    )
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.RecordExpr
                                        [ Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "c"
                                            , Elm.Syntax.Node.empty
                                                Elm.Syntax.Expression.UnitExpr
                                            )
                                        , Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "b"
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Floatable 2.2)
                                            )
                                        ]
                                    )
                                ]
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeRecord
                                            (FastDict.fromList
                                                [ ( "c"
                                                  , ElmSyntaxTypeInfer.TypeNotVariable
                                                        ElmSyntaxTypeInfer.TypeUnit
                                                  )
                                                , ( "b"
                                                  , ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { arguments = []
                                                            , moduleOrigin = [ "Basics" ]
                                                            , name = "Float"
                                                            }
                                                        )
                                                  )
                                                ]
                                            )
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeRecord
                                                        (FastDict.fromList
                                                            [ ( "c"
                                                              , ElmSyntaxTypeInfer.TypeNotVariable
                                                                    ElmSyntaxTypeInfer.TypeUnit
                                                              )
                                                            , ( "b"
                                                              , ElmSyntaxTypeInfer.TypeNotVariable
                                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                                        { arguments = []
                                                                        , moduleOrigin = [ "Basics" ]
                                                                        , name = "Float"
                                                                        }
                                                                    )
                                                              )
                                                            ]
                                                        )
                                                    )
                                                ]
                                            }
                                        )
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
        |> Result.map .type_
        |> Expect.equal
            (Ok expectedInferredType)


exampleModuleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookup =
    []
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes
