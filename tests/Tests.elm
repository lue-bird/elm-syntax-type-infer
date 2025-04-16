module Tests exposing (suite)

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import ElmSyntaxTypeInfer
import Expect
import FastDict
import Test exposing (Test)


suite : Test
suite =
    Test.describe "ElmSyntaxTypeInfer"
        [ Test.test "readme example"
            (\() ->
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
                    |> Result.andThen
                        (\declarationsTyped ->
                            case declarationsTyped |> FastDict.get "majorVersions" of
                                Nothing ->
                                    Err "typed declaration not found"

                                Just majorVersionDeclarationTyped ->
                                    Ok majorVersionDeclarationTyped.type_
                        )
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
        , Test.test "ElmSyntaxTypeInfer documentation example"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "hello"
                            , expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Literal "world")
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
                    |> Result.andThen
                        (\declarationsTyped ->
                            case declarationsTyped |> FastDict.get "hello" of
                                Nothing ->
                                    Err "typed declaration not found"

                                Just majorVersionDeclarationTyped ->
                                    Ok majorVersionDeclarationTyped.type_
                        )
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "String" ], name = "String", arguments = [] }
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
                                                        , recordVariable = "record"
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
                                    ElmSyntaxTypeInfer.TypeVariable "number1"
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
        , Test.test "argument pattern variable unified with Float \\(a) -> [ a, 2.2 ]"
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
        , Test.test "argument pattern variable as if-then-else condition \\a -> if a then () else ()"
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
                                    Elm.Syntax.Expression.UnitExpr
                                )
                                (Elm.Syntax.Node.empty
                                    Elm.Syntax.Expression.UnitExpr
                                )
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
                                            , name = "Bool"
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        ElmSyntaxTypeInfer.TypeUnit
                                }
                            )
                        )
            )
        , Test.test "argument pattern variable unified with unit in if-then-else branch \\a -> if a then () else ()"
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
                                    (Elm.Syntax.Expression.FunctionOrValue [] "True")
                                )
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                )
                                (Elm.Syntax.Node.empty
                                    Elm.Syntax.Expression.UnitExpr
                                )
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        ElmSyntaxTypeInfer.TypeUnit
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        ElmSyntaxTypeInfer.TypeUnit
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
                                            { recordVariable = "record"
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
        , Test.test "directly applied lambda with inexhaustive record pattern: (\\{ a } -> a) ({ a = 1.1, b = \"\" })"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.LambdaExpression
                            { args =
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.RecordPattern
                                        [ Elm.Syntax.Node.empty "a" ]
                                    )
                                ]
                            , expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                            }
                        )
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.RecordExpr
                            [ Elm.Syntax.Node.empty
                                ( Elm.Syntax.Node.empty "a"
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Floatable 1.1)
                                )
                            , Elm.Syntax.Node.empty
                                ( Elm.Syntax.Node.empty "b"
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Literal "")
                                )
                            ]
                        )
                    ]
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "Basics" ]
                                    , name = "Float"
                                    , arguments = []
                                    }
                                )
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
        , Test.test "record update union with same record variable \\(rec) -> [ { rec | a = (), b = 1 }, { rec | c = (), b = 2.2 } ]"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.ParenthesizedPattern
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "rec")
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.RecordUpdateExpression
                                        (Elm.Syntax.Node.empty "rec")
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
                                        (Elm.Syntax.Node.empty "rec")
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
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeRecordExtension
                                                { recordVariable = "rec"
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
                                                            { recordVariable = "rec"
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
            )
        , let
            recordExtensionTypeInExample : ElmSyntaxTypeInfer.Type String
            recordExtensionTypeInExample =
                ElmSyntaxTypeInfer.TypeNotVariable
                    (ElmSyntaxTypeInfer.TypeRecordExtension
                        { recordVariable = "x"
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
          in
          Test.test "record update union with record update over different record variable \\x y -> [ { x | a = (), b = 1 }, { y | c = (), b = 2.2 } ]"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "x")
                        , Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "y")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.RecordUpdateExpression
                                        (Elm.Syntax.Node.empty "x")
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
                                        (Elm.Syntax.Node.empty "y")
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
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = recordExtensionTypeInExample
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeFunction
                                                { input = recordExtensionTypeInExample
                                                , output =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { moduleOrigin = [ "List" ]
                                                            , name = "List"
                                                            , arguments =
                                                                [ recordExtensionTypeInExample ]
                                                            }
                                                        )
                                                }
                                            )
                                    }
                                )
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
        , Test.test "bad matched + pattern unification should fail: case [] of [ 1 ] -> 1; 0 -> 0"
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
                                (Elm.Syntax.Pattern.IntPattern 0)
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Integer 0)
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.err
            )
        , Test.test "bad matched + pattern unification should fail: case [] of [ \"\" ] -> 1; [ 0 ] -> 0"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr [])
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.ListPattern
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.StringPattern "")
                                    ]
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Integer 1)
                          )
                        , ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.ListPattern
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.IntPattern 0)
                                    ]
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Integer 0)
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.err
            )
        , Test.test "bad matched + pattern unification should fail: case [] of [ \"\" ] -> 1; _ -> \"\""
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr [])
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.ListPattern
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Pattern.StringPattern "")
                                    ]
                                )
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Integer 1)
                          )
                        , ( Elm.Syntax.Node.empty
                                Elm.Syntax.Pattern.AllPattern
                          , Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Literal "")
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.err
            )
        , Test.test "fully connected case in and output type: case [] of n -> n"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.ListExpr [])
                    , cases =
                        [ ( Elm.Syntax.Node.empty
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
                                        [ ElmSyntaxTypeInfer.TypeVariable "element"
                                        ]
                                    }
                                )
                            )
                        )
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
        , Test.test "same generic pattern in pattern: case ( 1.1, \"\" ) of ( _, _ ) -> ()"
            (\() ->
                Elm.Syntax.Expression.CaseExpression
                    { expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.TupledExpression
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Floatable 1.1)
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Literal "")
                                ]
                            )
                    , cases =
                        [ ( Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.TuplePattern
                                    [ Elm.Syntax.Node.empty
                                        Elm.Syntax.Pattern.AllPattern
                                    , Elm.Syntax.Node.empty
                                        Elm.Syntax.Pattern.AllPattern
                                    ]
                                )
                          , Elm.Syntax.Node.empty
                                Elm.Syntax.Expression.UnitExpr
                          )
                        ]
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                ElmSyntaxTypeInfer.TypeUnit
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
        , Test.test "infer matched via variant patterns: \\order -> case order of Basics.LT -> -1 ; EQ -> 0 ; GT -> 1"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "order")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.CaseExpression
                                { expression =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "order")
                                , cases =
                                    [ ( Elm.Syntax.Node.empty
                                            (Elm.Syntax.Pattern.NamedPattern
                                                { moduleName = [ "Basics" ]
                                                , name = "LT"
                                                }
                                                []
                                            )
                                      , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.Integer -1)
                                      )
                                    , ( Elm.Syntax.Node.empty
                                            (Elm.Syntax.Pattern.NamedPattern
                                                { moduleName = []
                                                , name = "EQ"
                                                }
                                                []
                                            )
                                      , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.Integer 0)
                                      )
                                    , ( Elm.Syntax.Node.empty
                                            (Elm.Syntax.Pattern.NamedPattern
                                                { moduleName = []
                                                , name = "GT"
                                                }
                                                []
                                            )
                                      , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.Integer 1)
                                      )
                                    ]
                                }
                            )
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { arguments = []
                                                , moduleOrigin = [ "Basics" ]
                                                , name = "Order"
                                                }
                                            )
                                    , output =
                                        ElmSyntaxTypeInfer.TypeVariable "number"
                                    }
                                )
                            )
                        )
            )
        , Test.test "infer via variant pattern argument and using as pattern: \\maybe -> case maybe of Just 0 as just0 -> just0 ; other -> other"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "maybe")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.CaseExpression
                                { expression =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "maybe")
                                , cases =
                                    [ ( Elm.Syntax.Node.empty
                                            (Elm.Syntax.Pattern.AsPattern
                                                (Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Pattern.NamedPattern
                                                        { moduleName = []
                                                        , name = "Just"
                                                        }
                                                        [ Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Pattern.IntPattern 0)
                                                        ]
                                                    )
                                                )
                                                (Elm.Syntax.Node.empty "just0")
                                            )
                                      , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "just0")
                                      )
                                    , ( Elm.Syntax.Node.empty
                                            (Elm.Syntax.Pattern.VarPattern "other")
                                      , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "other")
                                      )
                                    ]
                                }
                            )
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Maybe" ]
                                                , name = "Maybe"
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
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Maybe" ]
                                                , name = "Maybe"
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
                                    }
                                )
                            )
                        )
            )
        , Test.test "extract record fields in destructuring: let { x, y } = { x = \"\", y = 1.1 } in x"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetDestructuring
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.RecordPattern
                                        [ Elm.Syntax.Node.empty "x"
                                        , Elm.Syntax.Node.empty "y"
                                        ]
                                    )
                                )
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.RecordExpr
                                        [ Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "x"
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Literal "")
                                            )
                                        , Elm.Syntax.Node.empty
                                            ( Elm.Syntax.Node.empty "y"
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Floatable 1.1)
                                            )
                                        ]
                                    )
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "x")
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "String" ]
                                    , name = "String"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "extract tuple parts in destructuring of call: let ( x, y ) = Tuple.pair \"\" 1.1 in x"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetDestructuring
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.TuplePattern
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Pattern.VarPattern "x")
                                        , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Pattern.VarPattern "y")
                                        ]
                                    )
                                )
                                (Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Application
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [ "Tuple" ] "pair")
                                        , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.Literal "")
                                        , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.Floatable 1.1)
                                        ]
                                    )
                                )
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "x")
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "String" ]
                                    , name = "String"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "determine lambda parameter type by unification with pattern in let destructuring: \\a -> let () = a in a"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "a")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.LetDestructuring
                                            (Elm.Syntax.Node.empty
                                                Elm.Syntax.Pattern.UnitPattern
                                            )
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                            )
                                        )
                                    ]
                                , expression =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                }
                            )
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            ElmSyntaxTypeInfer.TypeUnit
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            ElmSyntaxTypeInfer.TypeUnit
                                    }
                                )
                            )
                        )
            )
        , Test.test "curried call: Tuple.pair \"\""
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Tuple" ] "pair")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Literal "")
                    ]
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = ElmSyntaxTypeInfer.TypeVariable "b"
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeTuple
                                                { part0 =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { arguments = []
                                                            , moduleOrigin = [ "String" ]
                                                            , name = "String"
                                                            }
                                                        )
                                                , part1 = ElmSyntaxTypeInfer.TypeVariable "b"
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "curried call: Tuple.pair <| \"\""
            (\() ->
                Elm.Syntax.Expression.OperatorApplication
                    "<|"
                    Elm.Syntax.Infix.Right
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Tuple" ] "pair")
                    )
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Literal "")
                    )
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = ElmSyntaxTypeInfer.TypeVariable "b"
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeTuple
                                                { part0 =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { arguments = []
                                                            , moduleOrigin = [ "String" ]
                                                            , name = "String"
                                                            }
                                                        )
                                                , part1 = ElmSyntaxTypeInfer.TypeVariable "b"
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "curried call: \"\" |> Tuple.pair"
            (\() ->
                Elm.Syntax.Expression.OperatorApplication
                    "|>"
                    Elm.Syntax.Infix.Left
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Literal "")
                    )
                    (Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Tuple" ] "pair")
                    )
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = ElmSyntaxTypeInfer.TypeVariable "b"
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeTuple
                                                { part0 =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { arguments = []
                                                            , moduleOrigin = [ "String" ]
                                                            , name = "String"
                                                            }
                                                        )
                                                , part1 = ElmSyntaxTypeInfer.TypeVariable "b"
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied implicitly imported variant: Just 1.1"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [] "Just")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 1.1)
                    ]
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookupImportingProcess
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "Maybe" ]
                                    , name = "Maybe"
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
        , let
            moduleOriginLookupImportingId : ElmSyntaxTypeInfer.ModuleOriginLookup
            moduleOriginLookupImportingId =
                [ Elm.Syntax.Node.empty
                    { moduleName = Elm.Syntax.Node.empty [ "Id" ]
                    , moduleAlias = Nothing
                    , exposingList =
                        Just
                            (Elm.Syntax.Node.empty
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Id"
                                            , open = Just Elm.Syntax.Range.empty
                                            }
                                        )
                                    ]
                                )
                            )
                    }
                ]
                    |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                        declarationTypes

            declarationTypes : FastDict.Dict Elm.Syntax.ModuleName.ModuleName ElmSyntaxTypeInfer.ModuleTypes
            declarationTypes =
                ElmSyntaxTypeInfer.elmCoreTypes
                    |> FastDict.insert
                        [ "Id" ]
                        ([ Elm.Syntax.Declaration.CustomTypeDeclaration
                            { documentation = Nothing
                            , name = Elm.Syntax.Node.empty "Id"
                            , generics = []
                            , constructors =
                                [ Elm.Syntax.Node.empty
                                    { name = Elm.Syntax.Node.empty "Id"
                                    , arguments =
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "Int" ))
                                                []
                                            )
                                        ]
                                    }
                                ]
                            }
                         ]
                            |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                exampleModuleOriginLookup
                            |> .types
                        )
          in
          Test.test "fully applied explicitly imported variant: module Id exposing (Id(..)) ; type Id = Id Int ;; import Id exposing (Id(..)) ; id = Id 1"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [] "Id")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Integer 1)
                    ]
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = declarationTypes
                        , moduleOriginLookup = moduleOriginLookupImportingId
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    moduleOriginLookupImportingId
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "Id" ]
                                    , name = "Id"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , let
            moduleOriginLookupImportingId : ElmSyntaxTypeInfer.ModuleOriginLookup
            moduleOriginLookupImportingId =
                [ Elm.Syntax.Node.empty
                    { moduleName = Elm.Syntax.Node.empty [ "Id" ]
                    , moduleAlias = Just (Elm.Syntax.Node.empty [ "Ident" ])
                    , exposingList =
                        Just
                            (Elm.Syntax.Node.empty
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Id"
                                            , open = Just Elm.Syntax.Range.empty
                                            }
                                        )
                                    ]
                                )
                            )
                    }
                ]
                    |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                        declarationTypes

            declarationTypes : FastDict.Dict Elm.Syntax.ModuleName.ModuleName ElmSyntaxTypeInfer.ModuleTypes
            declarationTypes =
                ElmSyntaxTypeInfer.elmCoreTypes
                    |> FastDict.insert
                        [ "Id" ]
                        ([ Elm.Syntax.Declaration.CustomTypeDeclaration
                            { documentation = Nothing
                            , name = Elm.Syntax.Node.empty "Id"
                            , generics = []
                            , constructors =
                                [ Elm.Syntax.Node.empty
                                    { name = Elm.Syntax.Node.empty "Id"
                                    , arguments =
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "Int" ))
                                                []
                                            )
                                        ]
                                    }
                                ]
                            }
                         ]
                            |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                exampleModuleOriginLookup
                            |> .types
                        )
          in
          Test.test "fully applied explicitly imported variant, used as exposed but imported also using alias: module Id exposing (Id(..)) ; type Id = Id Int ;; import Id exposing (Id(..)) ; id = Id 1"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [] "Id")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Integer 1)
                    ]
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = declarationTypes
                        , moduleOriginLookup = moduleOriginLookupImportingId
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    moduleOriginLookupImportingId
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "Id" ]
                                    , name = "Id"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied implicitly locally declared variant with multiple values: type Two = Two String Float ; two = Two \"\" 1.1"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [] "Two")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Literal "")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 1.1)
                    ]
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "Two"
                                , generics = []
                                , constructors =
                                    [ Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "Two"
                                        , arguments =
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "String" ))
                                                    []
                                                )
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "Float" ))
                                                    []
                                                )
                                            ]
                                        }
                                    ]
                                }
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookup
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = []
                                    , name = "Two"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "pattern of locally declared variant with multiple values: type Two = Two String Float ; two = \\(Two string float) -> string"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.NamedPattern
                                { moduleName = []
                                , name = "Two"
                                }
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "string")
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "float")
                                ]
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "string")
                    }
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "Two"
                                , generics = []
                                , constructors =
                                    [ Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "Two"
                                        , arguments =
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "String" ))
                                                    []
                                                )
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "Float" ))
                                                    []
                                                )
                                            ]
                                        }
                                    ]
                                }
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookup
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = []
                                                , name = "Two"
                                                , arguments = []
                                                }
                                            )
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "String" ]
                                                , name = "String"
                                                , arguments = []
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied imported call: Process.sleep 1.1"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Process" ] "sleep")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Floatable 1.1)
                    ]
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookupImportingProcess
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "Task" ]
                                    , name = "Task"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeVariable "x"
                                        , ElmSyntaxTypeInfer.TypeNotVariable
                                            ElmSyntaxTypeInfer.TypeUnit
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied imported call with incorrectly typed argument: Process.sleep \"\""
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.FunctionOrValue [ "Process" ] "sleep")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Literal "")
                    ]
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookupImportingProcess
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.err
            )
        , Test.test "curried appendable prefix operation: (++) \"\""
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.PrefixOperator "++")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Literal "")
                    ]
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { arguments = []
                                                , moduleOrigin = [ "String" ]
                                                , name = "String"
                                                }
                                            )
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { arguments = []
                                                , moduleOrigin = [ "String" ]
                                                , name = "String"
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied appendable prefix operation with different types: (++) \"\" []"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.PrefixOperator "++")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.Literal "")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.ListExpr [])
                    ]
                    |> expressionToInferredType
                    |> Expect.err
            )
        , Test.test "fully applied appendable prefix operation with unifiable list types: (++) [ \"\" ] []"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.PrefixOperator "++")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.ListExpr
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Literal "")
                            ]
                        )
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.ListExpr [])
                    ]
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
                                                { arguments = []
                                                , moduleOrigin = [ "String" ]
                                                , name = "String"
                                                }
                                            )
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied appendable prefix operation with non-unifiable list types: (++) [ \"\" ] [ 0 ]"
            (\() ->
                Elm.Syntax.Expression.Application
                    [ Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.PrefixOperator "++")
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.ListExpr
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Literal "")
                            ]
                        )
                    , Elm.Syntax.Node.empty
                        (Elm.Syntax.Expression.ListExpr
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Integer 0)
                            ]
                        )
                    ]
                    |> expressionToInferredType
                    |> Expect.err
            )
        , Test.test "fully applied record type alias constructor for one field, no generics"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Application
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "Record")
                                        , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.Literal "")
                                        ]
                                    )
                            , name = Elm.Syntax.Node.empty "constructedRecord"
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
                            [ Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "Record"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Record
                                            [ Elm.Syntax.Node.empty
                                                ( Elm.Syntax.Node.empty "field"
                                                , Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                )
                                            ]
                                        )
                                }
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookup
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = []
                                    , name = "Record"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "local type aliases with same name as variant from different module: type alias Just a = a ; just : Just (Just String) ; just = \"\""
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Literal "")
                            , name = Elm.Syntax.Node.empty "just"
                            , arguments = []
                            }
                  , signature =
                        Just
                            (Elm.Syntax.Node.empty
                                { name = Elm.Syntax.Node.empty "just"
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Typed
                                            (Elm.Syntax.Node.empty ( [], "Just" ))
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "Just" ))
                                                    [ Elm.Syntax.Node.empty
                                                        (Elm.Syntax.TypeAnnotation.Typed
                                                            (Elm.Syntax.Node.empty ( [], "String" ))
                                                            []
                                                        )
                                                    ]
                                                )
                                            ]
                                        )
                                }
                            )
                  , documentation = Nothing
                  }
                ]
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "Just"
                                , generics = [ Elm.Syntax.Node.empty "a" ]
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.GenericType "a")
                                }
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookup
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = []
                                    , name = "Just"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = []
                                                , name = "Just"
                                                , arguments =
                                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { moduleOrigin = [ "String" ]
                                                            , name = "String"
                                                            , arguments = []
                                                            }
                                                        )
                                                    ]
                                                }
                                            )
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "recursive, too strictly annotated function: addAbs : Int -> Int -> Int ; addAbs toAdd base = if toAdd <= 0 then base else 1 + addAbs (toAdd - 1) base"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "addAbs"
                            , arguments =
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "toAdd")
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "base")
                                ]
                            , expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.IfBlock
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.OperatorApplication
                                                "<="
                                                Elm.Syntax.Infix.Non
                                                (Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.FunctionOrValue [] "toAdd")
                                                )
                                                (Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.Integer 0)
                                                )
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "base")
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.OperatorApplication
                                                "+"
                                                Elm.Syntax.Infix.Left
                                                (Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.Integer 1)
                                                )
                                                (Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.Application
                                                        [ Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.FunctionOrValue [] "addAbs")
                                                        , Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.OperatorApplication
                                                                "-"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.FunctionOrValue [] "base")
                                                                )
                                                                (Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                            )
                                                        , Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.Integer 1)
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    )
                            }
                  , signature =
                        Just
                            (Elm.Syntax.Node.empty
                                { name = Elm.Syntax.Node.empty "addAbs"
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "Int" ))
                                                    []
                                                )
                                            )
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                    (Elm.Syntax.Node.empty
                                                        (Elm.Syntax.TypeAnnotation.Typed
                                                            (Elm.Syntax.Node.empty ( [], "Int" ))
                                                            []
                                                        )
                                                    )
                                                    (Elm.Syntax.Node.empty
                                                        (Elm.Syntax.TypeAnnotation.Typed
                                                            (Elm.Syntax.Node.empty ( [], "Int" ))
                                                            []
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                }
                            )
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
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Basics" ]
                                                , name = "Int"
                                                , arguments = []
                                                }
                                            )
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeFunction
                                                { input =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { moduleOrigin = [ "Basics" ]
                                                            , name = "Int"
                                                            , arguments = []
                                                            }
                                                        )
                                                , output =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { moduleOrigin = [ "Basics" ]
                                                            , name = "Int"
                                                            , arguments = []
                                                            }
                                                        )
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "same name for parameter and imported but one is qualified: stringResizePadLeftWith0s : Int -> String -> String ; stringResizePadLeftWith0s length unpaddedString = if length < (unpaddedString |> String.length) then \"\" else unpaddedString"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "stringResizePadLeftWith0s"
                            , arguments =
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "length")
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "unpaddedString")
                                ]
                            , expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.IfBlock
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.OperatorApplication
                                                "<"
                                                Elm.Syntax.Infix.Non
                                                (Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.FunctionOrValue [] "length")
                                                )
                                                (Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.OperatorApplication
                                                        "|>"
                                                        Elm.Syntax.Infix.Left
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.FunctionOrValue [] "unpaddedString")
                                                        )
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.FunctionOrValue [ "String" ] "length")
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.Literal "")
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "unpaddedString")
                                        )
                                    )
                            }
                  , signature =
                        Just
                            (Elm.Syntax.Node.empty
                                { name = Elm.Syntax.Node.empty "stringResizePadLeftWith0s"
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "Int" ))
                                                    []
                                                )
                                            )
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                    (Elm.Syntax.Node.empty
                                                        (Elm.Syntax.TypeAnnotation.Typed
                                                            (Elm.Syntax.Node.empty ( [], "String" ))
                                                            []
                                                        )
                                                    )
                                                    (Elm.Syntax.Node.empty
                                                        (Elm.Syntax.TypeAnnotation.Typed
                                                            (Elm.Syntax.Node.empty ( [], "String" ))
                                                            []
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                }
                            )
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
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Basics" ]
                                                , name = "Int"
                                                , arguments = []
                                                }
                                            )
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeFunction
                                                { input =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { moduleOrigin = [ "String" ]
                                                            , name = "String"
                                                            , arguments = []
                                                            }
                                                        )
                                                , output =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { moduleOrigin = [ "String" ]
                                                            , name = "String"
                                                            , arguments = []
                                                            }
                                                        )
                                                }
                                            )
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
        , Test.test "single annotated let value declaration: let a : Float ; a = 2.2 in a"
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
                                , signature =
                                    Just
                                        (Elm.Syntax.Node.empty
                                            { name = Elm.Syntax.Node.empty "a"
                                            , typeAnnotation =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty
                                                            ( [], "Float" )
                                                        )
                                                        []
                                                    )
                                            }
                                        )
                                , documentation = Nothing
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "Basics" ]
                                    , name = "Float"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "single annotated let function declaration with multiple arguments, called: let a : Float -> String -> () ; a x y = () in a 1.1 \"\""
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "a"
                                        , arguments =
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.Pattern.VarPattern "x")
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Pattern.VarPattern "y")
                                            ]
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                Elm.Syntax.Expression.UnitExpr
                                        }
                                , signature =
                                    Just
                                        (Elm.Syntax.Node.empty
                                            { name = Elm.Syntax.Node.empty "a"
                                            , typeAnnotation =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.TypeAnnotation.Typed
                                                                (Elm.Syntax.Node.empty
                                                                    ( [], "Float" )
                                                                )
                                                                []
                                                            )
                                                        )
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                                (Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                                        (Elm.Syntax.Node.empty
                                                                            ( [], "String" )
                                                                        )
                                                                        []
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.empty
                                                                    Elm.Syntax.TypeAnnotation.Unit
                                                                )
                                                            )
                                                        )
                                                    )
                                            }
                                        )
                                , documentation = Nothing
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Floatable 1.1)
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Literal "")
                                ]
                            )
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                ElmSyntaxTypeInfer.TypeUnit
                            )
                        )
            )
        , Test.test "single annotated let function declaration with multiple arguments: let a : Float ; a = 2.2 in a"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "a"
                                        , arguments =
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.Pattern.VarPattern "x")
                                            ]
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue [] "x")
                                        }
                                , signature =
                                    Just
                                        (Elm.Syntax.Node.empty
                                            { name = Elm.Syntax.Node.empty "a"
                                            , typeAnnotation =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.TypeAnnotation.Typed
                                                                (Elm.Syntax.Node.empty
                                                                    ( [], "Float" )
                                                                )
                                                                []
                                                            )
                                                        )
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.TypeAnnotation.Typed
                                                                (Elm.Syntax.Node.empty
                                                                    ( [], "Float" )
                                                                )
                                                                []
                                                            )
                                                        )
                                                    )
                                            }
                                        )
                                , documentation = Nothing
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                    }
                    |> expressionToInferredType
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Basics" ]
                                                , name = "Float"
                                                , arguments = []
                                                }
                                            )
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = [ "Basics" ]
                                                , name = "Float"
                                                , arguments = []
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "single un-annotated let declaration getting its type from unification: \\a -> let b = [ a, 2.2 ] in a"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "a")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.LetFunction
                                            { declaration =
                                                Elm.Syntax.Node.empty
                                                    { name = Elm.Syntax.Node.empty "b"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.ListExpr
                                                                [ Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.Floatable 2.2)
                                                                , Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                                                ]
                                                            )
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
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "unknown lambda parameter getting its type directly from annotated let: \\a -> let b : Float ; b = a in a"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "a")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.LetFunction
                                            { declaration =
                                                Elm.Syntax.Node.empty
                                                    { name = Elm.Syntax.Node.empty "b"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                                    }
                                            , signature =
                                                Just
                                                    (Elm.Syntax.Node.empty
                                                        { name = Elm.Syntax.Node.empty "b"
                                                        , typeAnnotation =
                                                            Elm.Syntax.Node.empty
                                                                (Elm.Syntax.TypeAnnotation.Typed
                                                                    (Elm.Syntax.Node.empty ( [ "Basics" ], "Float" ))
                                                                    []
                                                                )
                                                        }
                                                    )
                                            , documentation = Nothing
                                            }
                                        )
                                    ]
                                , expression =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                }
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "unifying different imported type aliases to the same type from annotated let: let noop : Platform.ProcessId -> Process.Id ; noop id = id in ()"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "noop"
                                        , arguments =
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.Pattern.VarPattern "id")
                                            ]
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue [] "id")
                                        }
                                , signature =
                                    Just
                                        (Elm.Syntax.Node.empty
                                            { name = Elm.Syntax.Node.empty "noop"
                                            , typeAnnotation =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.TypeAnnotation.Typed
                                                                (Elm.Syntax.Node.empty ( [ "Platform" ], "ProcessId" ))
                                                                []
                                                            )
                                                        )
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.TypeAnnotation.Typed
                                                                (Elm.Syntax.Node.empty ( [ "Process" ], "Id" ))
                                                                []
                                                            )
                                                        )
                                                    )
                                            }
                                        )
                                , documentation = Nothing
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.TupledExpression [])
                    }
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookupImportingProcess
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                ElmSyntaxTypeInfer.TypeUnit
                            )
                        )
            )
        , Test.test "unifying different local (and imported) type aliases to the same type from annotated let: let noop : String -> StringToo ; noop id = id in ()"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "noop"
                                        , arguments =
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.Pattern.VarPattern "id")
                                            ]
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue [] "id")
                                        }
                                , signature =
                                    Just
                                        (Elm.Syntax.Node.empty
                                            { name = Elm.Syntax.Node.empty "noop"
                                            , typeAnnotation =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.TypeAnnotation.Typed
                                                                (Elm.Syntax.Node.empty ( [], "String" ))
                                                                []
                                                            )
                                                        )
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.TypeAnnotation.Typed
                                                                (Elm.Syntax.Node.empty ( [], "StringToo" ))
                                                                []
                                                            )
                                                        )
                                                    )
                                            }
                                        )
                                , documentation = Nothing
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.TupledExpression [])
                    }
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "StringToo"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Typed
                                            (Elm.Syntax.Node.empty ( [], "String" ))
                                            []
                                        )
                                }
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookup
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                ElmSyntaxTypeInfer.TypeUnit
                            )
                        )
            )
        , Test.test "single un-annotated let declaration getting its type from unification with annotated let: \\a -> let b : Float ; b = a ; c = a in a"
            (\() ->
                Elm.Syntax.Expression.LambdaExpression
                    { args =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Pattern.VarPattern "a")
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.LetFunction
                                            { declaration =
                                                Elm.Syntax.Node.empty
                                                    { name = Elm.Syntax.Node.empty "b"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                                    }
                                            , signature =
                                                Just
                                                    (Elm.Syntax.Node.empty
                                                        { name = Elm.Syntax.Node.empty "b"
                                                        , typeAnnotation =
                                                            Elm.Syntax.Node.empty
                                                                (Elm.Syntax.TypeAnnotation.Typed
                                                                    (Elm.Syntax.Node.empty ( [ "Basics" ], "Float" ))
                                                                    []
                                                                )
                                                        }
                                                    )
                                            , documentation = Nothing
                                            }
                                        )
                                    , Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.LetFunction
                                            { declaration =
                                                Elm.Syntax.Node.empty
                                                    { name = Elm.Syntax.Node.empty "c"
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
                                        (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                }
                            )
                    }
                    |> expressionExpectInferredType
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                        )
            )
        , Test.test "single incorrectly annotated let declaration let a : Int ; a = 2.2 in a"
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
                                , signature =
                                    Just
                                        (Elm.Syntax.Node.empty
                                            { name = Elm.Syntax.Node.empty "a"
                                            , typeAnnotation =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [ "Basics" ], "Int" ))
                                                        []
                                                    )
                                            }
                                        )
                                , documentation = Nothing
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                    }
                    |> expressionToInferredType
                    |> Expect.err
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
        , Test.test "transitive un-annotated top level declaration: a = 2.2; b = a"
            (\() ->
                [ { declaration =
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
                , { declaration =
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
                ]
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "StringToo"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Typed
                                            (Elm.Syntax.Node.empty ( [], "String" ))
                                            []
                                        )
                                }
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookup
                                |> .types
                        }
                    |> Result.map
                        (\declarationsTyped ->
                            declarationsTyped
                                |> FastDict.map (\_ -> .type_)
                                |> FastDict.toList
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "a"
                              , ElmSyntaxTypeInfer.TypeNotVariable
                                    (ElmSyntaxTypeInfer.TypeConstruct
                                        { moduleOrigin = [ "Basics" ]
                                        , name = "Float"
                                        , arguments = []
                                        }
                                    )
                              )
                            , ( "b"
                              , ElmSyntaxTypeInfer.TypeNotVariable
                                    (ElmSyntaxTypeInfer.TypeConstruct
                                        { moduleOrigin = [ "Basics" ]
                                        , name = "Float"
                                        , arguments = []
                                        }
                                    )
                              )
                            ]
                        )
            )
        , Test.test "mutually influencing un-annotated top level declarations: a = 2 + b; b = a"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "a"
                            , arguments = []
                            , expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.OperatorApplication
                                        "+"
                                        Elm.Syntax.Infix.Left
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.Integer 2)
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "b")
                                        )
                                    )
                            }
                  , signature = Nothing
                  , documentation = Nothing
                  }
                , { declaration =
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
                ]
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "StringToo"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Typed
                                            (Elm.Syntax.Node.empty ( [], "String" ))
                                            []
                                        )
                                }
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    exampleModuleOriginLookup
                                |> .types
                        }
                    |> Result.map
                        (\declarationsTyped ->
                            declarationsTyped
                                |> FastDict.map (\_ -> .type_)
                                |> FastDict.toList
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "a", ElmSyntaxTypeInfer.TypeVariable "numberDeclarationResult" )
                            , ( "b", ElmSyntaxTypeInfer.TypeVariable "numberA" )
                            ]
                        )
            )
        , Test.test "self-referential a union with list of a \\a -> [ a, [ a ] ] should fail"
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
                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                , Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.ListExpr
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                        ]
                                    )
                                ]
                            )
                    }
                    |> expressionToInferredType
                    |> Expect.err
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


expressionWrapInExampleDeclaration :
    Elm.Syntax.Expression.Expression
    -> Elm.Syntax.Expression.Function
expressionWrapInExampleDeclaration expression =
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


expressionToInferredType :
    Elm.Syntax.Expression.Expression
    -> Result String (ElmSyntaxTypeInfer.Type String)
expressionToInferredType expression =
    [ expressionWrapInExampleDeclaration expression ]
        |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
            { importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
            , moduleOriginLookup = exampleModuleOriginLookup
            , otherModuleDeclaredTypes =
                []
                    |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                        exampleModuleOriginLookup
                    |> .types
            }
        |> Result.andThen toSingleInferredDeclaration


toSingleInferredDeclaration : FastDict.Dict String { inferred_ | type_ : type_ } -> Result String type_
toSingleInferredDeclaration declarationsInferred =
    case declarationsInferred |> FastDict.getMin of
        Nothing ->
            Err "no typed declarations found"

        Just ( _, declarationTyped ) ->
            Ok declarationTyped.type_


exampleModuleOriginLookupImportingProcess : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookupImportingProcess =
    [ Elm.Syntax.Node.empty
        { moduleName = Elm.Syntax.Node.empty [ "Process" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    ]
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes


exampleModuleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookup =
    []
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes
