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
                                            "number"
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
        , Test.test "[ .a, .b ]"
            (\() ->
                Elm.Syntax.Expression.ListExpr
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.RecordAccessFunction ".a")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.RecordAccessFunction ".b")
                    ]
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeRecordExtension
                                                        { fields =
                                                            FastDict.fromList
                                                                [ ( "b"
                                                                  , ElmSyntaxTypeInfer.TypeVariable "a"
                                                                  )
                                                                , ( "a"
                                                                  , ElmSyntaxTypeInfer.TypeVariable "a"
                                                                  )
                                                                ]
                                                        , recordVariable = "base"
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeVariable "a"
                                            }
                                        )
                                    ]
                                , moduleOrigin = [ "List" ]
                                , name = "List"
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
        , Test.test "(independent) integers and float in triple ( 1, 2.2, 3 )"
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
                                    ElmSyntaxTypeInfer.TypeVariable "number"
                                , part1 =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , part2 =
                                    ElmSyntaxTypeInfer.TypeVariable "number2"
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
                        (ElmSyntaxTypeInfer.TypeVariable "number")
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
                                                [ ElmSyntaxTypeInfer.TypeVariable "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable "a"
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
                                    ElmSyntaxTypeInfer.TypeVariable "a"
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
                                    ElmSyntaxTypeInfer.TypeVariable "number"
                                , output =
                                    ElmSyntaxTypeInfer.TypeVariable "number"
                                }
                            )
                        )
            )
        , Test.test "argument pattern variable called in Basics.negate parenthesized, implicit import \\(a) -> Basics.negate (a)"
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
                                        "number"
                                , output =
                                    ElmSyntaxTypeInfer.TypeVariable
                                        "number"
                                }
                            )
                        )
            )
        , Test.test "argument pattern variable accessed field, implicit import \\a -> a.field"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "a")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.RecordAccess
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                        )
                                    )
                                )
                                (Elm.Syntax.Node.empty "field")
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeRecordExtension
                                            { recordVariable = "recordWithField"
                                            , fields =
                                                FastDict.singleton "field"
                                                    (ElmSyntaxTypeInfer.TypeVariable
                                                        "field"
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeVariable "field"
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
                                    ElmSyntaxTypeInfer.TypeVariable "number"
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable "number"
                                                ]
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "argument pattern variable equivalent to number variable \\a -> if a then a else a"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "a")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.IfBlock
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                )
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                )
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                )
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
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
                                            { recordVariable = "base"
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
                                                        { recordVariable = "base"
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
        , Test.test "case 2 of 1 -> 1; n -> n"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.Integer 2)
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.IntPattern 1)
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Integer 2)
                          )
                        , ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "n")
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.FunctionOrValue [] "n")
                          )
                        ]
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { arguments = []
                                , moduleOrigin = [ "Basics" ]
                                , name = "Int"
                                }
                            )
                        )
            )
        , Test.test "should fail: case [] of [ 1 ] -> 1; n -> n"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr [])
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.ListPattern
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.IntPattern 1)
                                    ]
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Integer 1)
                          )
                        , ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "n")
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.FunctionOrValue [] "n")
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.err
            )
        , Test.test "case [] of [ 1 ] -> [ 2 ]; n -> n"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr [])
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.ListPattern
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.IntPattern 1)
                                    ]
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.ListExpr
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.Integer 2)
                                    ]
                                )
                          )
                        , ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "n")
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.FunctionOrValue [] "n")
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "List" ]
                                    , name = "List"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Basics" ]
                                                , name = "Int"
                                                , arguments = []
                                                }
                                            )
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "case [] of [ first, 1 ] -> [ first ]; n -> n"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr [])
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.ListPattern
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.VarPattern "first")
                                    , Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.IntPattern 1)
                                    ]
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.ListExpr
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "first")
                                    ]
                                )
                          )
                        , ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "n")
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.FunctionOrValue [] "n")
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "List" ]
                                    , name = "List"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Basics" ]
                                                , name = "Int"
                                                , arguments = []
                                                }
                                            )
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "case [] of first :: 1 :: _ -> [ first ]; n -> n"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr [])
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.UnConsPattern
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.VarPattern "first")
                                    )
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.UnConsPattern
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.Pattern.IntPattern 1)
                                            )
                                            (Elm.Syntax.Node.empty
                                                Elm.Syntax.Pattern.AllPattern
                                            )
                                        )
                                    )
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.ListExpr
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "first")
                                    ]
                                )
                          )
                        , ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "n")
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.FunctionOrValue [] "n")
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "List" ]
                                    , name = "List"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Basics" ]
                                                , name = "Int"
                                                , arguments = []
                                                }
                                            )
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "case [] of first :: _ -> [ 2.2 ]; n -> n"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr [])
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.UnConsPattern
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.VarPattern "first")
                                    )
                                    (Elm.Syntax.Node.empty
                                        Elm.Syntax.Pattern.AllPattern
                                    )
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.ListExpr
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.Floatable 2.2)
                                    ]
                                )
                          )
                        , ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "n")
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.FunctionOrValue [] "n")
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
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
            )
        , Test.test "single un-annotated let declaration let a = 2.2 in a"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "a"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Floatable 2.2)
                                        }
                                , signature = Nothing
                                , documentation = Nothing
                                }
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
        , Test.test "transitive un-annotated let declaration let a = 2.2; b = a in b"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "a"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Floatable 2.2)
                                        }
                                , signature = Nothing
                                , documentation = Nothing
                                }
                            )
                        , Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "b"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                        }
                                , signature = Nothing
                                , documentation = Nothing
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "b")
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
        ]


expressionExpectInferredType :
    ElmSyntaxTypeInfer.Type String
    -> Elm.Syntax.Expression.Expression
    -> Expect.Expectation
expressionExpectInferredType expectedInferredType expression =
    expression
        |> expressionToInferredType
        |> Expect.equal
            (Ok expectedInferredType)


expressionToInferredType :
    Elm.Syntax.Expression.Expression
    ->
        Result
            String
            (ElmSyntaxTypeInfer.Type String)
expressionToInferredType expression =
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
        |> Result.map
            (\declarationInferred ->
                declarationInferred.type_
            )


exampleModuleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookup =
    []
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes
