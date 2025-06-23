module Tests exposing (suite)

import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Module
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen
                        (\declarationsTyped ->
                            case declarationsTyped |> List.head of
                                Nothing ->
                                    Err "typed declaration not found"

                                Just majorVersionDeclarationTyped ->
                                    Ok majorVersionDeclarationTyped.type_
                        )
                    |> Expect.equal
                        (Ok
                            (typeList
                                (ElmSyntaxTypeInfer.TypeVariable
                                    { useRange = Elm.Syntax.Range.empty
                                    , name = "number"
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen
                        (\declarationsTyped ->
                            case declarationsTyped |> List.head of
                                Nothing ->
                                    Err "typed declaration not found"

                                Just majorVersionDeclarationTyped ->
                                    Ok majorVersionDeclarationTyped.type_
                        )
                    |> Expect.equal
                        (Ok
                            typeString
                        )
            )
        , Test.test "unify integer and float in list"
            (\() ->
                """module A exposing (..)
accessors = [ 1, 2.2 ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (typeList typeFloat)
                        )
            )
        , Test.test "record accessor functions unified: [ .a, .b ]"
            (\() ->
                """module A exposing (..)
accessors = [ .a, .b ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (typeList
                                (ElmSyntaxTypeInfer.TypeNotVariable
                                    (ElmSyntaxTypeInfer.TypeFunction
                                        { input =
                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeRecordExtension
                                                    { fields =
                                                        FastDict.fromList
                                                            [ ( "b"
                                                              , ElmSyntaxTypeInfer.TypeVariable
                                                                    { useRange = { end = { column = 21, row = 2 }, start = { column = 16, row = 2 } }
                                                                    , name = "a"
                                                                    }
                                                              )
                                                            , ( "a"
                                                              , ElmSyntaxTypeInfer.TypeVariable
                                                                    { useRange = { end = { column = 21, row = 2 }, start = { column = 16, row = 2 } }
                                                                    , name = "a"
                                                                    }
                                                              )
                                                            ]
                                                    , recordVariable =
                                                        { useRange = { end = { column = 20, row = 2 }, start = { column = 15, row = 2 } }
                                                        , name = "record"
                                                        }
                                                    }
                                                )
                                        , output =
                                            ElmSyntaxTypeInfer.TypeVariable
                                                { useRange = { end = { column = 21, row = 2 }, start = { column = 16, row = 2 } }
                                                , name = "a"
                                                }
                                        }
                                    )
                                )
                            )
                        )
            )
        , Test.test "unify integer and float in addition"
            (\() ->
                """module A exposing (..)
float = 1 + 2.2
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeFloat
                        )
            )
        , Test.test "(independent) integers and float in triple ( 1, 2.2, 3 )"
            (\() ->
                """module A exposing (..)
numbers = ( 1, 2.2, 3 )
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeTriple
                                    { part0 =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 14, row = 2 }, start = { column = 13, row = 2 } }
                                            , name = "number"
                                            }
                                    , part1 = typeFloat
                                    , part2 =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 22, row = 2 }, start = { column = 21, row = 2 } }
                                            , name = "number1"
                                            }
                                    }
                                )
                            )
                        )
            )
        , Test.test "identity called with float (from implicit import)"
            (\() ->
                """module A exposing (..)
doubleTwo = identity 2.2
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeFloat
                        )
            )
        , Test.test "identity called with integer (from implicit import)"
            (\() ->
                """module A exposing (..)
doubleTwo = identity 1
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeVariable
                                { useRange = { end = { column = 23, row = 2 }, start = { column = 13, row = 2 } }
                                , name = "number"
                                }
                            )
                        )
            )
        , Test.test "Basics.identity called with float (qualified from implicit import)"
            (\() ->
                """module A exposing (..)
doubleTwo = Basics.identity 2.2
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeFloat
                        )
            )
        , Test.test "Basics.identity <| float (qualified from implicit import)"
            (\() ->
                """module A exposing (..)
doubleTwo = Basics.identity <| 2.2
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeFloat
                        )
            )
        , Test.test "float |> Basics.identity (qualified from implicit import)"
            (\() ->
                """module A exposing (..)
doubleTwo = 2.2 |> Basics.identity
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeFloat
                        )
            )
        , Test.test "List.map called with Basics.identity (qualified from implicit import)"
            (\() ->
                """module A exposing (..)
doubleTwo = List.map Basics.identity
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        typeList
                                            (ElmSyntaxTypeInfer.TypeVariable
                                                { useRange = { end = { column = 37, row = 2 }, start = { column = 13, row = 2 } }
                                                , name = "a"
                                                }
                                            )
                                    , output =
                                        typeList
                                            (ElmSyntaxTypeInfer.TypeVariable
                                                { useRange = { end = { column = 37, row = 2 }, start = { column = 13, row = 2 } }
                                                , name = "a"
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "let (a) = 2.2 in a"
            (\() ->
                """module A exposing (..)
doubleTwo = let (a) = 2.2 in a
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeFloat
                        )
            )
        , Test.test "case 2.2 of (a) -> a"
            (\() ->
                """module A exposing (..)
doubleTwo = case 2.2 of (a) -> a
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeFloat
                        )
            )
        , Test.test "unused argument pattern variable \\(a) -> [ 1, 2.2 ]"
            (\() ->
                """module A exposing (..)
alwaysSomeFloats = \\(a) -> [ 1, 2.2 ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 23, row = 2 }, start = { column = 22, row = 2 } }
                                            , name = "a"
                                            }
                                    , output = typeList typeFloat
                                    }
                                )
                            )
                        )
            )
        , Test.test "argument pattern variable unified with Float \\(a) -> [ a, 2.2 ]"
            (\() ->
                """module A exposing (..)
listTwoWithTwoPointTwo = \\(a) -> [ a, 2.2 ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeFloat
                                    , output = typeList typeFloat
                                    }
                                )
                            )
                        )
            )
        , Test.test "argument pattern variable unified with unit in if-then-else branch \\a -> if True then a else ()"
            (\() ->
                """module A exposing (..)
unitIdentity = \\a -> if True then a else ()
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
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
        , Test.test "argument pattern variable as condition in if-then-else branch \\a -> if a then () else ()"
            (\() ->
                """module A exposing (..)
boolToUnit = \\a -> if a then () else ()
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeBool
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            ElmSyntaxTypeInfer.TypeUnit
                                    }
                                )
                            )
                        )
            )
        , Test.test "argument pattern variable negated parenthesized \\(a) -> -(a)"
            (\() ->
                """module A exposing (..)
negateAlias = \\(a) -> -(a)
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 27, row = 2 }, start = { column = 17, row = 2 } }
                                            , name = "number"
                                            }
                                    , output =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 27, row = 2 }, start = { column = 17, row = 2 } }
                                            , name = "number"
                                            }
                                    }
                                )
                            )
                        )
            )
        , Test.test "argument pattern variable called in Basics.negate parenthesized, implicit import \\(a) -> Basics.negate (a)"
            (\() ->
                """module A exposing (..)
negateAlias = \\(a) -> Basics.negate (a)
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 40, row = 2 }, start = { column = 17, row = 2 } }
                                            , name = "number"
                                            }
                                    , output =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 40, row = 2 }, start = { column = 17, row = 2 } }
                                            , name = "number"
                                            }
                                    }
                                )
                            )
                        )
            )
        , Test.test "argument pattern variable accessed field, implicit import \\a -> a.field"
            (\() ->
                """module A exposing (..)
recordAccessField = \\a -> a.field
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeRecordExtension
                                                { recordVariable =
                                                    { useRange = { end = { column = 29, row = 2 }, start = { column = 28, row = 2 } }
                                                    , name = "record"
                                                    }
                                                , fields =
                                                    FastDict.singleton "field"
                                                        (ElmSyntaxTypeInfer.TypeVariable
                                                            { useRange = { end = { column = 34, row = 2 }, start = { column = 29, row = 2 } }
                                                            , name = "field"
                                                            }
                                                        )
                                                }
                                            )
                                    , output =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 34, row = 2 }, start = { column = 29, row = 2 } }
                                            , name = "field"
                                            }
                                    }
                                )
                            )
                        )
            )
        , Test.test "directly applied lambda with inexhaustive record pattern: (\\{ a } -> a) ({ a = 1.1, b = \"\" })"
            (\() ->
                """module A exposing (..)
onePointOne = (\\{ a } -> a) ({ a = 1.1, b = "" })
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok typeFloat)
            )
        , Test.test "directly applied lambda with inexhaustive record pattern, unifying from the other side: { a = 1.1, b = \"\" } |> (\\{ a } -> a)"
            (\() ->
                """module A exposing (..)
onePointOne = { a = 1.1, b = "" } |> (\\{ a } -> a)
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok typeFloat)
            )
        , Test.test "directly applied lambda with inexhaustive record pattern of field not present in the record is invalid: (\\{ c } -> c) ({ a = 1.1, b = \"\" })"
            (\() ->
                """module A exposing (..)
onePointOne = (\\{ c } -> c) ({ a = 1.1, b = "" })
"""
                    |> typeInferModuleFromSource
                    |> Expect.err
            )
        , Test.test "fully applied function with inexhaustive record pattern: toStringWithRecord (\\{ b } -> b)"
            (\() ->
                """module A exposing (..)
toStringWithRecord : ({ a: Float, b : String } -> String) -> String
toStringWithRecord recordToString =
    recordToString { a = 1.1, b = "" }

stringEmpty = toStringWithRecord (\\{ b } -> b)
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok typeString)
            )
        , Test.test "fully applied function unifying type alias record with inexhaustive record pattern: toStringWithRecord (\\{ b } -> b)"
            (\() ->
                """module A exposing (..)
type alias AB = { a: Float, b : String }
toStringWithRecord : (AB -> String) -> String
toStringWithRecord recordToString =
    recordToString { a = 1.1, b = "" }

stringEmpty = toStringWithRecord (\\{ b } -> b)
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok typeString)
            )
        , Test.test "fully applied 3-parameter function unifying type alias record with inexhaustive record pattern"
            (\() ->
                """module A exposing (..)
type alias Match =
    { match : String
    , index : Int
    , number : Int
    , submatches : List (Maybe String)
    }

replace : String -> (Match -> String) -> String -> String
replace _ matchToReplacement _ =
    matchToReplacement
        { match = ""
        , index = 0
        , number = 0
        , submatches = []
        }

stringEmpty string =
    replace ""
        (\\{ submatches } ->
            ""
        )
        string
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeString
                                    , output = typeString
                                    }
                                )
                            )
                        )
            )
        , Test.test "induce record pattern substitution by applying function unifying type alias record with inexhaustive record pattern and pattern matching on the pattern variable"
            (\() ->
                """module A exposing (..)

replace : ({ a : String, b : List (Maybe String) } -> String) -> String
replace matchToReplacement = ""

stringEmpty =
    replace
        (\\{ b } ->
            case b of
                (Just match) :: _ -> ""
                _ -> ""
        )
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok typeString)
            )
        , Test.test "fully applied function unifying type alias record with usage of only one of it's fields"
            (\() ->
                """module A exposing (..)
type alias AB = { a: Float, b : String }
toStringWithRecord : (AB -> String) -> String
toStringWithRecord recordToString =
    recordToString { a = 1.1, b = "" }

stringEmpty = toStringWithRecord (\\rec -> rec.b)
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok typeString)
            )
        , Test.test "fully applied function unifying type alias record with usage of only some of it's fields"
            (\() ->
                """module A exposing (..)
type alias AB = { a: Float, b : String, c : String }
toStringWithRecord : (AB -> x) -> x
toStringWithRecord recordToX =
    recordToX { a = 1.1, b = "", c = "" }

stringEmpty = toStringWithRecord (\\rec -> ( rec.b, rec.a ))
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeTuple
                                    { part0 = typeString
                                    , part1 = typeFloat
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied function unifying type alias record with usage of only one of it's fields, input record unified with a variable used as output and input"
            (\() ->
                """module A exposing (..)
type alias AB = { a: Float, b : String, c : String }
toStringWithRecord : (AB -> x) -> (x -> y) -> y
toStringWithRecord recordToX xToY =
    xToY (recordToX { a = 1.1, b = "", c = "" })

stringEmpty = toStringWithRecord (\\rec -> rec) .b
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeString
                        )
            )
        , Test.test "fully applied function unifying record with usage of only some of it's fields"
            (\() ->
                """module A exposing (..)
toStringWithRecord : ({ a: Bool, b : String, c : String } -> x) -> x
toStringWithRecord recordToX =
    recordToX { a = True, b = "", c = "" }

stringEmpty = toStringWithRecord (\\rec -> if rec.a then rec else rec)
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeRecord
                                    (FastDict.fromList
                                        [ ( "b", typeString )
                                        , ( "a", typeBool )
                                        , ( "c", typeString )
                                        ]
                                    )
                                )
                            )
                        )
            )
        , Test.test "fully applied function unifying record with usage of only some of it's fields, input record unified with a variable used as output and input"
            (\() ->
                """module A exposing (..)
toStringWithRecord : ({ a: Bool, b : String, c : String } -> x) -> (x -> y) -> y
toStringWithRecord = Debug.todo ""

stringEmpty = toStringWithRecord (\\rec -> if rec.a then rec else rec) .b
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeString
                        )
            )
        , Test.test "fully applied function unifying type alias record with usage of only some of it's fields, input record unified with a variable used as output and input"
            (\() ->
                """module A exposing (..)
type alias AB = { a: Bool, b : String, c : String }
toStringWithRecord : (AB -> x) -> (x -> y) -> y
toStringWithRecord recordToX xToY =
    xToY (recordToX { a = True, b = "", c = "" })

stringEmpty = toStringWithRecord (\\rec -> if rec.a then rec else rec) .b
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.drop 1)
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeString
                        )
            )
        , Test.test "argument pattern variable equivalent to number variable \\(a) -> [ a, 1 ]"
            (\() ->
                """module A exposing (..)
onePointOne = \\(a) -> [ a, 1 ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 29, row = 2 }, start = { column = 17, row = 2 } }
                                            , name = "number"
                                            }
                                    , output =
                                        typeList
                                            (ElmSyntaxTypeInfer.TypeVariable
                                                { useRange = { end = { column = 29, row = 2 }, start = { column = 17, row = 2 } }
                                                , name = "number"
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "argument pattern variable equivalent to number variable \\a -> if a then a else a"
            (\() ->
                """module A exposing (..)
aaaa = \\a -> if a then a else a
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeBool
                                    , output = typeBool
                                    }
                                )
                            )
                        )
            )
        , Test.test "record update union with same record variable \\(rec) -> [ { rec | a = (), b = 1 }, { rec | c = (), b = 2.2 } ]"
            (\() ->
                """module A exposing (..)
records = \\(rec) -> [ { rec | a = (), b = 1 }, { rec | c = (), b = 2.2 } ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeRecordExtension
                                                { recordVariable =
                                                    { useRange = { end = { column = 73, row = 2 }, start = { column = 23, row = 2 } }
                                                    , name = "rec"
                                                    }
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
                                                        , ( "b", typeFloat )
                                                        ]
                                                }
                                            )
                                    , output =
                                        typeList
                                            (ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeRecordExtension
                                                    { recordVariable =
                                                        { useRange = { end = { column = 73, row = 2 }, start = { column = 23, row = 2 } }
                                                        , name = "rec"
                                                        }
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
                                                            , ( "b", typeFloat )
                                                            ]
                                                    }
                                                )
                                            )
                                    }
                                )
                            )
                        )
            )
        , let
            recordExtensionTypeInExample : ElmSyntaxTypeInfer.Type
            recordExtensionTypeInExample =
                ElmSyntaxTypeInfer.TypeNotVariable
                    (ElmSyntaxTypeInfer.TypeRecordExtension
                        { recordVariable =
                            { useRange = { end = { column = 67, row = 2 }, start = { column = 21, row = 2 } }
                            , name = "x"
                            }
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
                                , ( "b", typeFloat )
                                ]
                        }
                    )
          in
          Test.test "record update union with record update over different record variable \\x y -> [ { x | a = (), b = 1 }, { y | c = (), b = 2.2 } ]"
            (\() ->
                """module A exposing (..)
records = \\x y -> [ { x | a = (), b = 1 }, { y | c = (), b = 2.2 } ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
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
                                                    typeList recordExtensionTypeInExample
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "record update union with record \\a -> [ { a | b = 1 }, { c = (), b = 2.2 } ]"
            (\() ->
                """module A exposing (..)
records = \\a -> [ { a | b = 1 }, { c = (), b = 2.2 } ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
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
                                                    , ( "b", typeFloat )
                                                    ]
                                                )
                                            )
                                    , output =
                                        typeList
                                            (ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeRecord
                                                    (FastDict.fromList
                                                        [ ( "c"
                                                          , ElmSyntaxTypeInfer.TypeNotVariable
                                                                ElmSyntaxTypeInfer.TypeUnit
                                                          )
                                                        , ( "b", typeFloat )
                                                        ]
                                                    )
                                                )
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "case 2 of 1 -> 1; n -> n"
            (\() ->
                """module A exposing (..)
two =
    case 2 of
        1 -> 1
        n -> n
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok typeInt)
            )
        , Test.test "should fail: case [] of [ 1 ] -> 1; n -> n"
            (\() ->
                """module A exposing (..)
listEmpty =
    case [] of
        [ 1 ] -> 1
        n -> n
"""
                    |> typeInferModuleFromSource
                    |> Expect.err
            )
        , Test.test "bad matched + pattern unification should fail: case [] of [ 1 ] -> 1; 0 -> 0"
            (\() ->
                """module A exposing (..)
listEmpty =
    case [] of
        [ 1 ] -> 1
        0 -> 0
"""
                    |> typeInferModuleFromSource
                    |> Expect.err
            )
        , Test.test "bad matched + pattern unification should fail: case [] of [ \"\" ] -> 1; [ 0 ] -> 0"
            (\() ->
                """module A exposing (..)
listEmpty =
    case [] of
        [ "" ] -> 1
        [ 0 ] -> 0
"""
                    |> typeInferModuleFromSource
                    |> Expect.err
            )
        , Test.test "bad matched + pattern unification should fail: case [] of [ \"\" ] -> 1; _ -> \"\""
            (\() ->
                """module A exposing (..)
explode =
    case [] of
        [ "" ] -> 1
        _ -> ""
"""
                    |> typeInferModuleFromSource
                    |> Expect.err
            )
        , Test.test "fully connected case in and output type: case [] of n -> n"
            (\() ->
                """module A exposing (..)
listEmpty = case [] of n -> n
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (typeList
                                (ElmSyntaxTypeInfer.TypeVariable
                                    { useRange = { end = { column = 20, row = 2 }, start = { column = 18, row = 2 } }
                                    , name = "element"
                                    }
                                )
                            )
                        )
            )
        , Test.test "unify partially generic case result with concrete pattern: case [] of [ 1 ] -> [ 2 ]; n -> n"
            (\() ->
                """module A exposing (..)
single1To2 =
    case [] of
        [ 1 ] -> [ 2 ]
        n -> n
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (typeList typeInt)
                        )
            )
        , Test.test "case result by unifying with part of list exact pattern: case [] of [ first, 1 ] -> [ first ]; n -> n"
            (\() ->
                """module A exposing (..)
dropLast1If2Elements =
    case [] of
        [ first, 1 ] -> [ first ]
        n -> n
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (typeList typeInt)
                        )
            )
        , Test.test "same generic pattern in pattern: case ( 1.1, \"\" ) of ( _, _ ) -> ()"
            (\() ->
                """module A exposing (..)
waste = case ( 1.1, "" ) of ( _, _ ) -> ()
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                ElmSyntaxTypeInfer.TypeUnit
                            )
                        )
            )
        , Test.test "case result by unifying with part of list cons pattern: case [] of first :: 1 :: _ -> [ first ]; n -> n"
            (\() ->
                """module A exposing (..)
headSingleIfSecond1 =
    case [] of
        first :: 1 :: _ -> [ first ]
        n -> n
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (typeList typeInt)
                        )
            )
        , Test.test "generic matched unified with concrete case result: case [] of first :: _ -> [ 2.2 ]; n -> n"
            (\() ->
                """module A exposing (..)
filledFloatListElseSingleTwoDotTwo =
    case [] of
        first :: _ -> [ 2.2 ]
        n -> n
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (typeList typeFloat)
                        )
            )
        , Test.test "infer matched via variant patterns: \\order -> case order of Basics.LT -> -1 ; EQ -> 0 ; GT -> 1"
            (\() ->
                """module A exposing (..)
orderToInt =
    \\order ->
        case order of
            Basics.LT -> -1
            EQ -> 0
            GT -> 1
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeOrder
                                    , output =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 20, row = 7 }, start = { column = 26, row = 5 } }
                                            , name = "number"
                                            }
                                    }
                                )
                            )
                        )
            )
        , Test.test "infer via variant pattern argument and using as pattern: \\maybe -> case maybe of Just 0 as just0 -> just0 ; other -> other"
            (\() ->
                """module A exposing (..)
maybeIntIdentity =
    \\maybe ->
        case maybe of
            Just 0 as just0 -> just0
            other -> other
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeMaybe typeInt
                                    , output = typeMaybe typeInt
                                    }
                                )
                            )
                        )
            )
        , Test.test "extract record fields in destructuring: let { x, y } = { x = \"\", y = 1.1 } in x"
            (\() ->
                """module A exposing (..)
emptyString = let { x, y } = { x = "", y = 1.1 } in x
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeString
                        )
            )
        , Test.test "extract tuple parts in destructuring of call: let ( x, y ) = Tuple.pair \"\" 1.1 in x"
            (\() ->
                """module A exposing (..)
emptyString = let ( x, y ) = Tuple.pair "" 1.1 in x
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeString
                        )
            )
        , Test.test "determine lambda parameter type by unification with pattern in let destructuring: \\a -> let () = a in a"
            (\() ->
                """module A exposing (..)
unitIdentity = \\a -> let () = a in a
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
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
        , Test.test "let destructured pattern variable used in another destructuring: let (a) = 2.2 ; (b) = a in b"
            (\() ->
                """module A exposing (..)
doubleTrouble =
    let
        (a) = 2.2
        (b) = a
    in
    b
"""
                    |> typeInferModuleFromSource
                    |> Expect.equal
                        (Ok
                            [ { name = "doubleTrouble"
                              , type_ = typeFloat
                              , documentation = Nothing
                              , nameRange = { end = { column = 14, row = 2 }, start = { column = 1, row = 2 } }
                              , parameters = []
                              , result =
                                    { range = { end = { column = 6, row = 7 }, start = { column = 5, row = 3 } }
                                    , type_ = typeFloat
                                    , value =
                                        ElmSyntaxTypeInfer.ExpressionLetIn
                                            { declaration0 =
                                                { declaration =
                                                    ElmSyntaxTypeInfer.LetDestructuring
                                                        { expression =
                                                            { range = { end = { column = 18, row = 4 }, start = { column = 15, row = 4 } }
                                                            , type_ = typeFloat
                                                            , value = ElmSyntaxTypeInfer.ExpressionFloat 2.2
                                                            }
                                                        , pattern =
                                                            { range = { end = { column = 12, row = 4 }, start = { column = 9, row = 4 } }
                                                            , type_ = typeFloat
                                                            , value =
                                                                ElmSyntaxTypeInfer.PatternParenthesized
                                                                    { range = { end = { column = 11, row = 4 }, start = { column = 10, row = 4 } }
                                                                    , type_ = typeFloat
                                                                    , value = ElmSyntaxTypeInfer.PatternVariable "a"
                                                                    }
                                                            }
                                                        }
                                                , range = { end = { column = 18, row = 4 }, start = { column = 9, row = 4 } }
                                                }
                                            , declaration1Up =
                                                [ { declaration =
                                                        ElmSyntaxTypeInfer.LetDestructuring
                                                            { expression =
                                                                { range = { end = { column = 16, row = 5 }, start = { column = 15, row = 5 } }
                                                                , type_ = typeFloat
                                                                , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "", name = "a", qualification = "" }
                                                                }
                                                            , pattern =
                                                                { range = { end = { column = 12, row = 5 }, start = { column = 9, row = 5 } }
                                                                , type_ = typeFloat
                                                                , value =
                                                                    ElmSyntaxTypeInfer.PatternParenthesized
                                                                        { range = { end = { column = 11, row = 5 }, start = { column = 10, row = 5 } }
                                                                        , type_ = typeFloat
                                                                        , value = ElmSyntaxTypeInfer.PatternVariable "b"
                                                                        }
                                                                }
                                                            }
                                                  , range = { end = { column = 16, row = 5 }, start = { column = 9, row = 5 } }
                                                  }
                                                ]
                                            , result =
                                                { range = { end = { column = 6, row = 7 }, start = { column = 5, row = 7 } }
                                                , type_ = typeFloat
                                                , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "", name = "b", qualification = "" }
                                                }
                                            }
                                    }
                              , signature = Nothing
                              }
                            ]
                        )
            )
        , Test.test "curried call: Tuple.pair \"\""
            (\() ->
                """module A exposing (..)
tuple = Tuple.pair ""
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 19, row = 2 }, start = { column = 9, row = 2 } }
                                            , name = "b"
                                            }
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeTuple
                                                { part0 = typeString
                                                , part1 =
                                                    ElmSyntaxTypeInfer.TypeVariable
                                                        { useRange = { end = { column = 19, row = 2 }, start = { column = 9, row = 2 } }
                                                        , name = "b"
                                                        }
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "curried call: Tuple.pair <| \"\""
            (\() ->
                """module A exposing (..)
tuple = Tuple.pair <| ""
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 19, row = 2 }, start = { column = 9, row = 2 } }
                                            , name = "b"
                                            }
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeTuple
                                                { part0 = typeString
                                                , part1 =
                                                    ElmSyntaxTypeInfer.TypeVariable
                                                        { useRange = { end = { column = 19, row = 2 }, start = { column = 9, row = 2 } }
                                                        , name = "b"
                                                        }
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "curried call: \"\" |> Tuple.pair"
            (\() ->
                """module A exposing (..)
tuple = "" |> Tuple.pair
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 25, row = 2 }, start = { column = 15, row = 2 } }
                                            , name = "b"
                                            }
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeTuple
                                                { part0 = typeString
                                                , part1 =
                                                    ElmSyntaxTypeInfer.TypeVariable
                                                        { useRange = { end = { column = 25, row = 2 }, start = { column = 15, row = 2 } }
                                                        , name = "b"
                                                        }
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (typeMaybe typeFloat)
                        )
            )
        , Test.test "exposed member and annotated let declaration with the same name: let e : String ; e = \"\" ; in e"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { documentation = Nothing
                                , signature =
                                    Just
                                        (Elm.Syntax.Node.empty
                                            { name = Elm.Syntax.Node.empty "e"
                                            , typeAnnotation =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                            }
                                        )
                                , declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "e"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Literal "")
                                        }
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "e")
                    }
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeString
                        )
            )
        , Test.test "exposed member and un-annotated let declaration with the same name: let e = \"\" ; in e"
            (\() ->
                Elm.Syntax.Expression.LetExpression
                    { declarations =
                        [ Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.LetFunction
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "e"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Literal "")
                                        }
                                }
                            )
                        ]
                    , expression =
                        Elm.Syntax.Node.empty
                            (Elm.Syntax.Expression.FunctionOrValue [] "e")
                    }
                    |> expressionWrapInExampleDeclaration
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeString
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

            declarationTypes : FastDict.Dict String ElmSyntaxTypeInfer.ModuleTypes
            declarationTypes =
                ElmSyntaxTypeInfer.elmCoreTypes
                    |> FastDict.insert
                        "Id"
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
                            |> Elm.Syntax.Node.empty
                         ]
                            |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                { moduleName = "A"
                                , moduleOriginLookup = exampleModuleOriginLookup
                                }
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
                        { moduleName = "A"
                        , importedTypes = declarationTypes
                        , moduleOriginLookup = moduleOriginLookupImportingId
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = moduleOriginLookupImportingId
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Id"
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

            declarationTypes : FastDict.Dict String ElmSyntaxTypeInfer.ModuleTypes
            declarationTypes =
                ElmSyntaxTypeInfer.elmCoreTypes
                    |> FastDict.insert
                        "Id"
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
                            |> Elm.Syntax.Node.empty
                         ]
                            |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                { moduleName = "A"
                                , moduleOriginLookup = exampleModuleOriginLookup
                                }
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
                        { moduleName = "A"
                        , importedTypes = declarationTypes
                        , moduleOriginLookup = moduleOriginLookupImportingId
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = moduleOriginLookupImportingId
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Id"
                                    , name = "Id"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied implicitly locally declared variant with multiple values: type Two = Two String Float ; two = Two \"\" 1.1"
            (\() ->
                """module A exposing (..)
type Two = Two String Float
two = Two "" 1.1
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "A"
                                    , name = "Two"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied implicitly locally declared variant with multiple values whose types influence each other, one known"
            (\() ->
                """module A exposing (..)
type Both a = Both a a
two = \\second -> Both "" second
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeString
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = "A"
                                                , name = "Both"
                                                , arguments = [ typeString ]
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "fully applied implicitly locally declared variant with multiple values whose types influence each other, none known"
            (\() ->
                """module A exposing (..)
type Both a = Both a a
two = \\first second -> Both first second
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { name = "first", useRange = { end = { column = 28, row = 3 }, start = { column = 8, row = 3 } } }
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeFunction
                                                { input =
                                                    ElmSyntaxTypeInfer.TypeVariable
                                                        { name = "first", useRange = { end = { column = 28, row = 3 }, start = { column = 8, row = 3 } } }
                                                , output =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                            { moduleOrigin = "A"
                                                            , name = "Both"
                                                            , arguments =
                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                    { name = "first", useRange = { end = { column = 28, row = 3 }, start = { column = 8, row = 3 } } }
                                                                ]
                                                            }
                                                        )
                                                }
                                            )
                                    }
                                )
                            )
                        )
            )
        , Test.test "pattern of locally declared variant with multiple values whose types influence each other, one known"
            (\() ->
                """module A exposing (..)
type Both a = Both a a
two =
    \\both ->
        case both of
            Both "" second ->
                second
            
            Both _ second ->
                second
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = "A"
                                                , name = "Both"
                                                , arguments = [ typeString ]
                                                }
                                            )
                                    , output = typeString
                                    }
                                )
                            )
                        )
            )
        , Test.test "pattern of locally declared variant with multiple values whose types influence each other, none known"
            (\() ->
                """module A exposing (..)
type Both a = Both a a
two =
    \\(Both first second) -> ( first, second )
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = "A"
                                                , name = "Both"
                                                , arguments =
                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                        { name = "a", useRange = { end = { column = 24, row = 4 }, start = { column = 7, row = 4 } } }
                                                    ]
                                                }
                                            )
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeTuple
                                                { part0 =
                                                    ElmSyntaxTypeInfer.TypeVariable
                                                        { name = "a", useRange = { end = { column = 24, row = 4 }, start = { column = 7, row = 4 } } }
                                                , part1 =
                                                    ElmSyntaxTypeInfer.TypeVariable
                                                        { name = "a", useRange = { end = { column = 24, row = 4 }, start = { column = 7, row = 4 } } }
                                                }
                                            )
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
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
                                |> Elm.Syntax.Node.empty
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
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
                                                { moduleOrigin = "A"
                                                , name = "Two"
                                                , arguments = []
                                                }
                                            )
                                    , output = typeString
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Task"
                                    , name = "Task"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = Elm.Syntax.Range.empty
                                            , name = "x"
                                            }
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookupImportingProcess
                                    }
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
                                    { input = typeString
                                    , output = typeString
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
                            (typeList typeString)
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
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
                                |> Elm.Syntax.Node.empty
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "A"
                                    , name = "Record"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , let
            importedTypes : FastDict.Dict String ElmSyntaxTypeInfer.ModuleTypes
            importedTypes =
                FastDict.union
                    ElmSyntaxTypeInfer.elmCoreTypes
                    (FastDict.singleton "Imported"
                        ([ Elm.Syntax.Declaration.AliasDeclaration
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
                            |> Elm.Syntax.Node.empty
                         ]
                            |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                { moduleName = "A"
                                , moduleOriginLookup = exampleModuleOriginLookup
                                }
                            |> .types
                        )
                    )

            moduleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
            moduleOriginLookup =
                [ Elm.Syntax.Node.empty
                    { moduleName = Elm.Syntax.Node.empty [ "Imported" ]
                    , moduleAlias = Nothing
                    , exposingList =
                        Just
                            (Elm.Syntax.Node.empty
                                (Elm.Syntax.Exposing.All Elm.Syntax.Range.empty)
                            )
                    }
                ]
                    |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                        importedTypes
          in
          Test.test "fully applied imported (exposing(..)) record type alias constructor for one field, no generics"
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
                        { moduleName = "A"
                        , importedTypes = importedTypes
                        , moduleOriginLookup = moduleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = moduleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Imported"
                                    , name = "Record"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , let
            importedTypes : FastDict.Dict String ElmSyntaxTypeInfer.ModuleTypes
            importedTypes =
                FastDict.union
                    ElmSyntaxTypeInfer.elmCoreTypes
                    (FastDict.singleton "Imported"
                        ([ Elm.Syntax.Declaration.AliasDeclaration
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
                            |> Elm.Syntax.Node.empty
                         ]
                            |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                { moduleName = "A"
                                , moduleOriginLookup = exampleModuleOriginLookup
                                }
                            |> .types
                        )
                    )

            moduleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
            moduleOriginLookup =
                [ Elm.Syntax.Node.empty
                    { moduleName = Elm.Syntax.Node.empty [ "Imported" ]
                    , moduleAlias = Nothing
                    , exposingList =
                        Just
                            (Elm.Syntax.Node.empty
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Exposing.TypeOrAliasExpose "Record")
                                    ]
                                )
                            )
                    }
                ]
                    |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                        importedTypes
          in
          Test.test "fully applied imported (explicit exposing) record type alias constructor for one field, no generics"
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
                        { moduleName = "A"
                        , importedTypes = importedTypes
                        , moduleOriginLookup = moduleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = moduleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Imported"
                                    , name = "Record"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , let
            importedTypes : FastDict.Dict String ElmSyntaxTypeInfer.ModuleTypes
            importedTypes =
                FastDict.union
                    ElmSyntaxTypeInfer.elmCoreTypes
                    (FastDict.singleton "Imported"
                        ([ Elm.Syntax.Declaration.AliasDeclaration
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
                            |> Elm.Syntax.Node.empty
                         ]
                            |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                { moduleName = "A"
                                , moduleOriginLookup = exampleModuleOriginLookup
                                }
                            |> .types
                        )
                    )

            moduleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
            moduleOriginLookup =
                [ Elm.Syntax.Node.empty
                    { moduleName = Elm.Syntax.Node.empty [ "Imported" ]
                    , moduleAlias = Nothing
                    , exposingList = Nothing
                    }
                ]
                    |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                        importedTypes
          in
          Test.test "fully applied imported (not exposed) record type alias constructor for one field, no generics"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Application
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [ "Imported" ] "Record")
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
                        { moduleName = "A"
                        , importedTypes = importedTypes
                        , moduleOriginLookup = moduleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = moduleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Imported"
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
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
                                |> Elm.Syntax.Node.empty
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "A"
                                    , name = "Just"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                { moduleOrigin = "A"
                                                , name = "Just"
                                                , arguments =
                                                    [ typeString
                                                    ]
                                                }
                                            )
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "Char considered comparable 'a' < 'b'"
            (\() ->
                """module A exposing (..)
true = 'a' < 'b'
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeBool
                        )
            )
        , Test.test "local type alias used as comparable: type alias L = List String ; lt : L -> Bool ; lt l = l < l"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "lt"
                            , arguments =
                                [ Elm.Syntax.Node.empty
                                    (Elm.Syntax.Pattern.VarPattern "l")
                                ]
                            , expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.OperatorApplication
                                        "<"
                                        Elm.Syntax.Infix.Non
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "l")
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "l")
                                        )
                                    )
                            }
                  , signature =
                        Just
                            (Elm.Syntax.Node.empty
                                { name = Elm.Syntax.Node.empty "lt"
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "L" ))
                                                    []
                                                )
                                            )
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "Bool" ))
                                                    []
                                                )
                                            )
                                        )
                                }
                            )
                  , documentation = Nothing
                  }
                ]
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "L"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Typed
                                            (Elm.Syntax.Node.empty ( [], "List" ))
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "String" ))
                                                    []
                                                )
                                            ]
                                        )
                                }
                                |> Elm.Syntax.Node.empty
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
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
                                                { moduleOrigin = "A"
                                                , name = "L"
                                                , arguments = []
                                                }
                                            )
                                    , output = typeBool
                                    }
                                )
                            )
                        )
            )
        , Test.test "local type alias unified with imported choice type: type alias L = Dict.Dict String String ; l : L ; l = Dict.empty"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "l"
                            , arguments = []
                            , expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.FunctionOrValue [ "Dict" ] "empty")
                            }
                  , signature =
                        Just
                            (Elm.Syntax.Node.empty
                                { name = Elm.Syntax.Node.empty "l"
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Typed
                                            (Elm.Syntax.Node.empty ( [], "L" ))
                                            []
                                        )
                                }
                            )
                  , documentation = Nothing
                  }
                ]
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingDict
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "L"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Typed
                                            (Elm.Syntax.Node.empty ( [ "Dict" ], "Dict" ))
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "String" ))
                                                    []
                                                )
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "String" ))
                                                    []
                                                )
                                            ]
                                        )
                                }
                                |> Elm.Syntax.Node.empty
                            ]
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookupImportingDict
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "A"
                                    , name = "L"
                                    , arguments = []
                                    }
                                )
                            )
                        )
            )
        , Test.test "local type alias unified with variant value: type alias L = Dict.Dict String String ; type Wrap = Wrap L ; l = Wrap Dict.empty"
            (\() ->
                [ { declaration =
                        Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "l"
                            , arguments = []
                            , expression =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.Expression.Application
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [] "Wrap")
                                        , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.FunctionOrValue [ "Dict" ] "empty")
                                        ]
                                    )
                            }
                  , signature = Nothing
                  , documentation = Nothing
                  }
                ]
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookupImportingDict
                        , otherModuleDeclaredTypes =
                            [ Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "L"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.empty
                                        (Elm.Syntax.TypeAnnotation.Typed
                                            (Elm.Syntax.Node.empty ( [ "Dict" ], "Dict" ))
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "String" ))
                                                    []
                                                )
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "String" ))
                                                    []
                                                )
                                            ]
                                        )
                                }
                            , Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.empty "Wrap"
                                , generics = []
                                , constructors =
                                    [ Elm.Syntax.Node.empty
                                        { name = Elm.Syntax.Node.empty "Wrap"
                                        , arguments =
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.empty ( [], "L" ))
                                                    []
                                                )
                                            ]
                                        }
                                    ]
                                }
                            ]
                                |> List.map Elm.Syntax.Node.empty
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookupImportingDict
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "A"
                                    , name = "Wrap"
                                    , arguments = []
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeInt
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeFunction
                                                { input = typeInt
                                                , output = typeInt
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
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeInt
                                    , output =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            (ElmSyntaxTypeInfer.TypeFunction
                                                { input = typeString
                                                , output = typeString
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
                                        { name =
                                            Elm.Syntax.Node.Node
                                                { start = { row = 1, column = 1 }, end = { row = 1, column = 1 } }
                                                "a"
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
                        typeFloat
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
                            typeFloat
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
                                    { input = typeFloat
                                    , output = typeFloat
                                    }
                                )
                            )
                        )
            )
        , Test.test "single un-annotated let declaration getting its type from unification: \\a -> let b = [ a, 2.2 ] in a"
            (\() ->
                """module A exposing (..)
floatBeFloat = \\a -> let b = [ a, 2.2 ] in a
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeFloat
                                    , output = typeFloat
                                    }
                                )
                            )
                        )
            )
        , Test.test "unknown lambda parameter getting its type directly from annotated let: \\a -> let b : Float ; b = a in a"
            (\() ->
                """module A exposing (..)
allTheSame =
    \\a ->
        let b : Float
            b = a
        in
        a
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeFloat
                                    , output = typeFloat
                                    }
                                )
                            )
                        )
            )
        , Test.test "unifying different imported type aliases to the same type from annotated let: let noop : Platform.ProcessId -> Process.Id ; noop id = id in ()"
            (\() ->
                """module A exposing (..)
import Process
blub =
    let noop : Platform.ProcessId -> Process.Id
        noop id = id
    in ()
"""
                    |> typeInferModuleFromSource
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
                """module A exposing (..)
type alias StringToo = String
blub =
    let noop : String -> StringToo
        noop id = id
    in ()
"""
                    |> typeInferModuleFromSource
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
                """module A exposing (..)
allTheSame =
    \\a ->
        let b : Float
            b = a

            c = a
        in
        a
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeFloat
                                    , output = typeFloat
                                    }
                                )
                            )
                        )
            )
        , Test.test "self-recursive un-annotated let function declaration (single parameter)"
            (\() ->
                """module A exposing (..)
zero : Int -> Float
zero fullLength =
    let
        countdown index =
            if index < 0 then
                0.0

            else
                countdown (index - 1)
    in
    countdown (fullLength - 1)
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.ok
            )
        , Test.test "self-recursive un-annotated let function declaration (multiple parameters, simplified)"
            (\() ->
                """module A exposing (..)
listInitialize : Int -> (Int -> a) -> List a
listInitialize fullLength indexToElement =
    let
        step index soFar =
            if index < 0 then
                soFar

            else
                step (index - 1) soFar
    in
    step (fullLength - 1) []
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.ok
            )
        , Test.test "self-recursive un-annotated let function declaration (multiple parameters) not used"
            (\() ->
                """module A exposing (..)
listInitialize : ()
listInitialize =
    let
        step index soFar =
            if index < 0 then
                soFar

            else
                step (index - 1) (index :: soFar)
    in
    ()
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.ok
            )
        , Test.test "self-recursive un-annotated let function declaration (multiple parameters)"
            (\() ->
                """module A exposing (..)
listInitialize : Int -> List Int
listInitialize fullLength =
    let
        step index soFar =
            if index < 0 then
                soFar

            else
                step (index - 1) (index :: soFar)
    in
    step (fullLength - 1) []
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.ok
            )
        , Test.test "self-recursive annotated let function declaration (multiple parameters, using function from outer context)"
            (\() ->
                """module A exposing (..)
listInitialize : Int -> (Int -> a) -> List a
listInitialize fullLength indexToElement =
    let
        step : Int -> List a -> List a
        step index soFar =
            if index < 0 then
                soFar

            else
                step (index - 1) (indexToElement index :: soFar)
    in
    step (fullLength - 1) []
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.ok
            )
        , Test.test "self-recursive un-annotated let function declaration (multiple parameters, using function from outer context)"
            (\() ->
                """module A exposing (..)
listInitialize : Int -> (Int -> a) -> List a
listInitialize fullLength indexToElement =
    let
        step index soFar =
            if index < 0 then
                soFar

            else
                step (index - 1) (indexToElement index :: soFar)
    in
    step (fullLength - 1) []
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.ok
            )
            |> Test.skip
        , Test.test "single incorrectly annotated let declaration let a : Int ; a = 2.2 in a"
            (\() ->
                """module A exposing (..)
zwo =
    let a : Int
        a = 2.2
    in
    a
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.err
            )
        , Test.test "transitive un-annotated let declaration let a = 2.2; b = a in b"
            (\() ->
                """module A exposing (..)
zwo =
    let a = 2.2
        b = a
    in
    b
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            typeFloat
                        )
            )
        , Test.test "transitive un-annotated top level declarations: a = 2.2; b = a"
            (\() ->
                """module A exposing (..)
a = 2.2
b = a
"""
                    |> typeInferModuleFromSource
                    |> Result.map
                        (\declarationsTyped ->
                            declarationsTyped
                                |> List.map
                                    (\inferred ->
                                        ( inferred.name, inferred.type_ )
                                    )
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "a", typeFloat )
                            , ( "b", typeFloat )
                            ]
                        )
            )
        , Test.test "inner result types and module origins are consistent in transitive un-annotated top level declarations: a = 2.2; b = a"
            (\() ->
                """module A exposing (..)
a = 2.2
b = a
"""
                    |> typeInferModuleFromSource
                    |> Result.map
                        (\declarationsTyped ->
                            declarationsTyped
                                |> List.map
                                    (\inferred ->
                                        ( inferred.name, inferred.result )
                                    )
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "a"
                              , { type_ = typeFloat
                                , range = { end = { column = 8, row = 2 }, start = { column = 5, row = 2 } }
                                , value = ElmSyntaxTypeInfer.ExpressionFloat 2.2
                                }
                              )
                            , ( "b"
                              , { type_ = typeFloat
                                , range = { end = { column = 6, row = 3 }, start = { column = 5, row = 3 } }
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionReference
                                        { moduleOrigin = "A", name = "a", qualification = "" }
                                }
                              )
                            ]
                        )
            )
        , Test.test "mutually influencing un-annotated top level declarations: a = 2 + b; b = a"
            (\() ->
                """module A exposing (..)
a = 2 + b
b = a
"""
                    |> typeInferModuleFromSource
                    |> Result.map
                        (\declarationsTyped ->
                            declarationsTyped
                                |> List.map
                                    (\inferred ->
                                        ( inferred.name, inferred.type_ )
                                    )
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "a"
                              , ElmSyntaxTypeInfer.TypeVariable
                                    { useRange = { end = { column = 10, row = 2 }, start = { column = 5, row = 2 } }
                                    , name = "number"
                                    }
                              )
                            , ( "b"
                              , ElmSyntaxTypeInfer.TypeVariable
                                    { useRange = { end = { column = 6, row = 3 }, start = { column = 5, row = 3 } }
                                    , name = "number"
                                    }
                              )
                            ]
                        )
            )
        , Test.test "self-referential a union with list of a \\a -> [ a, [ a ] ] should fail"
            (\() ->
                """module A exposing (..)
impossible = \\a -> [ a, [ a ] ]
"""
                    |> typeInferModuleFromSource
                    |> Expect.err
            )
        , Test.test "inner types are consistent in List.map (\\a -> a) [ 2.2 ]"
            (\() ->
                """module A exposing (..)
majorVersions = List.map (\\a -> a) [ 2.2 ]
"""
                    |> typeInferModuleFromSource
                    |> Result.map
                        (\declarationsInferred ->
                            declarationsInferred
                                |> List.map .result
                        )
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = { end = { column = 43, row = 2 }, start = { column = 17, row = 2 } }
                                , type_ = typeList typeFloat
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionCall
                                        { argument0 =
                                            { range = { end = { column = 35, row = 2 }, start = { column = 26, row = 2 } }
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input = typeFloat, output = typeFloat }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionParenthesized
                                                    { range = { end = { column = 34, row = 2 }, start = { column = 27, row = 2 } }
                                                    , type_ =
                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                { input = typeFloat, output = typeFloat }
                                                            )
                                                    , value =
                                                        ElmSyntaxTypeInfer.ExpressionLambda
                                                            { parameter0 =
                                                                { range = { end = { column = 29, row = 2 }, start = { column = 28, row = 2 } }
                                                                , type_ = typeFloat
                                                                , value = ElmSyntaxTypeInfer.PatternVariable "a"
                                                                }
                                                            , parameter1Up = []
                                                            , result =
                                                                { range = { end = { column = 34, row = 2 }, start = { column = 33, row = 2 } }
                                                                , type_ = typeFloat
                                                                , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "", name = "a", qualification = "" }
                                                                }
                                                            }
                                                    }
                                            }
                                        , argument1Up =
                                            [ { range = { end = { column = 43, row = 2 }, start = { column = 36, row = 2 } }
                                              , type_ = typeList typeFloat
                                              , value =
                                                    ElmSyntaxTypeInfer.ExpressionList
                                                        [ { range = { end = { column = 41, row = 2 }, start = { column = 38, row = 2 } }
                                                          , type_ = typeFloat
                                                          , value = ElmSyntaxTypeInfer.ExpressionFloat 2.2
                                                          }
                                                        ]
                                              }
                                            ]
                                        , called =
                                            { range = { end = { column = 25, row = 2 }, start = { column = 17, row = 2 } }
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeFloat
                                                                    , output = typeFloat
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeList typeFloat
                                                                    , output = typeList typeFloat
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReference
                                                    { moduleOrigin = "List", name = "map", qualification = "List" }
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "inner types in annotated module-level declaration: eat : String -> () ; eat yum = ()"
            (\() ->
                """module A exposing (..)
eat : String -> ()
eat yum = ()
"""
                    |> typeInferModuleFromSource
                    |> Expect.equal
                        (Ok
                            [ { name = "eat"
                              , documentation = Nothing
                              , nameRange = { end = { column = 4, row = 3 }, start = { column = 1, row = 3 } }
                              , signature =
                                    Just
                                        { annotationType =
                                            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                (Elm.Syntax.Node.Node { end = { column = 13, row = 2 }, start = { column = 7, row = 2 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.Node { end = { column = 13, row = 2 }, start = { column = 7, row = 2 } } ( [], "String" ))
                                                        []
                                                    )
                                                )
                                                (Elm.Syntax.Node.Node { end = { column = 19, row = 2 }, start = { column = 17, row = 2 } }
                                                    Elm.Syntax.TypeAnnotation.Unit
                                                )
                                        , annotationTypeRange = { end = { column = 19, row = 2 }, start = { column = 7, row = 2 } }
                                        , nameRange = { end = { column = 4, row = 2 }, start = { column = 1, row = 2 } }
                                        , range = { end = { column = 19, row = 2 }, start = { column = 1, row = 2 } }
                                        }
                              , parameters =
                                    [ { range = { end = { column = 8, row = 3 }, start = { column = 5, row = 3 } }
                                      , type_ = typeString
                                      , value =
                                            ElmSyntaxTypeInfer.PatternVariable "yum"
                                      }
                                    ]
                              , type_ =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input = typeString
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    ElmSyntaxTypeInfer.TypeUnit
                                            }
                                        )
                              , result =
                                    { range = { end = { column = 13, row = 3 }, start = { column = 11, row = 3 } }
                                    , type_ =
                                        ElmSyntaxTypeInfer.TypeNotVariable
                                            ElmSyntaxTypeInfer.TypeUnit
                                    , value =
                                        ElmSyntaxTypeInfer.ExpressionUnit
                                    }
                              }
                            ]
                        )
            )
        , Test.test "inner types are consistent in listFloatIdentity : List Float -> List Float ; listFloatIdentity listFloat = List.map (\\a -> a) listFloat"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "listFloatIdentity"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "Float" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "Float" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                    )
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "listFloatIdentity"
                        , arguments =
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "listFloat")
                            ]
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Application
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                                    , Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.LambdaExpression
                                            { args =
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Pattern.VarPattern "a")
                                                ]
                                            , expression =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                            }
                                        )
                                    , Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue []
                                            "listFloat"
                                        )
                                    ]
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map
                        (\declarationsInferred ->
                            declarationsInferred
                                |> List.map
                                    (\inferred ->
                                        inferred.result
                                    )
                        )
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ = typeList typeFloat
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionCall
                                        { called =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeFloat, output = typeFloat }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeList typeFloat
                                                                    , output = typeList typeFloat
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReference
                                                    { moduleOrigin = "List"
                                                    , qualification = "List"
                                                    , name = "map"
                                                    }
                                            }
                                        , argument0 =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input = typeFloat, output = typeFloat }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionLambda
                                                    { parameter0 =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ = typeFloat
                                                        , value =
                                                            ElmSyntaxTypeInfer.PatternVariable "a"
                                                        }
                                                    , parameter1Up = []
                                                    , result =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ = typeFloat
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionReference
                                                                { moduleOrigin = ""
                                                                , qualification = ""
                                                                , name = "a"
                                                                }
                                                        }
                                                    }
                                            }
                                        , argument1Up =
                                            [ { range = Elm.Syntax.Range.empty
                                              , type_ = typeList typeFloat
                                              , value =
                                                    ElmSyntaxTypeInfer.ExpressionReference
                                                        { moduleOrigin = ""
                                                        , qualification = ""
                                                        , name = "listFloat"
                                                        }
                                              }
                                            ]
                                        }
                                }
                            )
                        )
            )
        , Test.test "inner types are consistent in unindent : List String -> List String ; unindent lines = List.map (\\line -> String.dropLeft 4 line) lines"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "unindent"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                    )
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "unindent"
                        , arguments =
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "lines")
                            ]
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Application
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                                    , Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.LambdaExpression
                                            { args =
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Pattern.VarPattern "line")
                                                ]
                                            , expression =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.Application
                                                        [ Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.FunctionOrValue [ "String" ] "dropLeft")
                                                        , Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.Integer 4)
                                                        , Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.FunctionOrValue [] "line")
                                                        ]
                                                    )
                                            }
                                        )
                                    , Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue []
                                            "lines"
                                        )
                                    ]
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map
                        (\declarationsInferred ->
                            declarationsInferred
                                |> List.map .result
                        )
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ = typeList typeString
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionCall
                                        { called =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeString
                                                                    , output = typeString
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeList typeString
                                                                    , output = typeList typeString
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReference
                                                    { moduleOrigin = "List"
                                                    , qualification = "List"
                                                    , name = "map"
                                                    }
                                            }
                                        , argument0 =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input = typeString, output = typeString }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionLambda
                                                    { parameter0 =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ = typeString
                                                        , value =
                                                            ElmSyntaxTypeInfer.PatternVariable "line"
                                                        }
                                                    , parameter1Up = []
                                                    , result =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ = typeString
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionCall
                                                                { called =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeInt
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeString
                                                                                            , output = typeString
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionReference
                                                                            { moduleOrigin = "String"
                                                                            , qualification = "String"
                                                                            , name = "dropLeft"
                                                                            }
                                                                    }
                                                                , argument0 =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ = typeInt
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionInteger
                                                                            { base = ElmSyntaxTypeInfer.Base10
                                                                            , value = 4
                                                                            }
                                                                    }
                                                                , argument1Up =
                                                                    [ { range = Elm.Syntax.Range.empty
                                                                      , type_ = typeString
                                                                      , value =
                                                                            ElmSyntaxTypeInfer.ExpressionReference
                                                                                { moduleOrigin = ""
                                                                                , qualification = ""
                                                                                , name = "line"
                                                                                }
                                                                      }
                                                                    ]
                                                                }
                                                        }
                                                    }
                                            }
                                        , argument1Up =
                                            [ { range = Elm.Syntax.Range.empty
                                              , type_ = typeList typeString
                                              , value =
                                                    ElmSyntaxTypeInfer.ExpressionReference
                                                        { moduleOrigin = ""
                                                        , qualification = ""
                                                        , name = "lines"
                                                        }
                                              }
                                            ]
                                        }
                                }
                            )
                        )
            )
        , Test.test "inner types are consistent in unindent : List String -> List String ; unindent lines = List.map (\\line -> line |> String.dropLeft 4) lines"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "unindent"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                    )
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "unindent"
                        , arguments =
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "lines")
                            ]
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.Application
                                    [ Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                                    , Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.LambdaExpression
                                            { args =
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Pattern.VarPattern "line")
                                                ]
                                            , expression =
                                                Elm.Syntax.Node.empty
                                                    (Elm.Syntax.Expression.OperatorApplication
                                                        "|>"
                                                        Elm.Syntax.Infix.Left
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.FunctionOrValue [] "line")
                                                        )
                                                        (Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.Application
                                                                [ Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.FunctionOrValue [ "String" ] "dropLeft")
                                                                , Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.Integer 4)
                                                                ]
                                                            )
                                                        )
                                                    )
                                            }
                                        )
                                    , Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue []
                                            "lines"
                                        )
                                    ]
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ = typeList typeString
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionCall
                                        { called =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeString
                                                                    , output = typeString
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeList typeString
                                                                    , output = typeList typeString
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReference
                                                    { moduleOrigin = "List"
                                                    , qualification = "List"
                                                    , name = "map"
                                                    }
                                            }
                                        , argument0 =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input = typeString, output = typeString }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionLambda
                                                    { parameter0 =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ = typeString
                                                        , value =
                                                            ElmSyntaxTypeInfer.PatternVariable "line"
                                                        }
                                                    , parameter1Up = []
                                                    , result =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ = typeString
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionInfixOperation
                                                                { operator =
                                                                    { moduleOrigin = "Basics"
                                                                    , symbol = "|>"
                                                                    , type_ =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeString
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input = typeString
                                                                                                        , output = typeString
                                                                                                        }
                                                                                                    )
                                                                                            , output = typeString
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                , left =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ = typeString
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionReference
                                                                            { moduleOrigin = ""
                                                                            , qualification = ""
                                                                            , name = "line"
                                                                            }
                                                                    }
                                                                , right =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeString
                                                                                , output = typeString
                                                                                }
                                                                            )
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionCall
                                                                            { called =
                                                                                { range = Elm.Syntax.Range.empty
                                                                                , type_ =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeInt
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input = typeString
                                                                                                        , output = typeString
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                , value =
                                                                                    ElmSyntaxTypeInfer.ExpressionReference
                                                                                        { moduleOrigin = "String"
                                                                                        , qualification = "String"
                                                                                        , name = "dropLeft"
                                                                                        }
                                                                                }
                                                                            , argument0 =
                                                                                { range = Elm.Syntax.Range.empty
                                                                                , type_ = typeInt
                                                                                , value =
                                                                                    ElmSyntaxTypeInfer.ExpressionInteger
                                                                                        { base = ElmSyntaxTypeInfer.Base10
                                                                                        , value = 4
                                                                                        }
                                                                                }
                                                                            , argument1Up = []
                                                                            }
                                                                    }
                                                                }
                                                        }
                                                    }
                                            }
                                        , argument1Up =
                                            [ { range = Elm.Syntax.Range.empty
                                              , type_ = typeList typeString
                                              , value =
                                                    ElmSyntaxTypeInfer.ExpressionReference
                                                        { moduleOrigin = ""
                                                        , qualification = ""
                                                        , name = "lines"
                                                        }
                                              }
                                            ]
                                        }
                                }
                            )
                        )
            )
        , Test.test "inner types are consistent in unindent : List String -> List String ; unindent lines = case True of True -> lines ; False -> List.map (\\line -> line |> String.dropLeft 4) lines"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "unindent"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                    )
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "unindent"
                        , arguments =
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "lines")
                            ]
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.IfBlock
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "True")
                                    )
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "lines")
                                    )
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.Application
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.LambdaExpression
                                                    { args =
                                                        [ Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Pattern.VarPattern "line")
                                                        ]
                                                    , expression =
                                                        Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.OperatorApplication
                                                                "|>"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.FunctionOrValue [] "line")
                                                                )
                                                                (Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.Application
                                                                        [ Elm.Syntax.Node.empty
                                                                            (Elm.Syntax.Expression.FunctionOrValue [ "String" ] "dropLeft")
                                                                        , Elm.Syntax.Node.empty
                                                                            (Elm.Syntax.Expression.Integer 4)
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                    }
                                                )
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue []
                                                    "lines"
                                                )
                                            ]
                                        )
                                    )
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ = typeList typeString
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionIfThenElse
                                        { condition =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ = typeBool
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReferenceVariant
                                                    { moduleOrigin = "Basics"
                                                    , choiceTypeName = "Bool"
                                                    , name = "True"
                                                    , qualification = ""
                                                    }
                                            }
                                        , onTrue =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ = typeList typeString
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReference
                                                    { moduleOrigin = ""
                                                    , name = "lines"
                                                    , qualification = ""
                                                    }
                                            }
                                        , onFalse =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ = typeList typeString
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionCall
                                                    { called =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeString
                                                                                , output = typeString
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeList typeString
                                                                                , output = typeList typeString
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionReference
                                                                { moduleOrigin = "List"
                                                                , qualification = "List"
                                                                , name = "map"
                                                                }
                                                        }
                                                    , argument0 =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeString, output = typeString }
                                                                )
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionLambda
                                                                { parameter0 =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ = typeString
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.PatternVariable "line"
                                                                    }
                                                                , parameter1Up = []
                                                                , result =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ = typeString
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionInfixOperation
                                                                            { operator =
                                                                                { moduleOrigin = "Basics"
                                                                                , symbol = "|>"
                                                                                , type_ =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeString
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input = typeString
                                                                                                                    , output = typeString
                                                                                                                    }
                                                                                                                )
                                                                                                        , output = typeString
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            , left =
                                                                                { range = Elm.Syntax.Range.empty
                                                                                , type_ = typeString
                                                                                , value =
                                                                                    ElmSyntaxTypeInfer.ExpressionReference
                                                                                        { moduleOrigin = ""
                                                                                        , qualification = ""
                                                                                        , name = "line"
                                                                                        }
                                                                                }
                                                                            , right =
                                                                                { range = Elm.Syntax.Range.empty
                                                                                , type_ =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeString
                                                                                            , output = typeString
                                                                                            }
                                                                                        )
                                                                                , value =
                                                                                    ElmSyntaxTypeInfer.ExpressionCall
                                                                                        { called =
                                                                                            { range = Elm.Syntax.Range.empty
                                                                                            , type_ =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input = typeInt
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input = typeString
                                                                                                                    , output = typeString
                                                                                                                    }
                                                                                                                )
                                                                                                        }
                                                                                                    )
                                                                                            , value =
                                                                                                ElmSyntaxTypeInfer.ExpressionReference
                                                                                                    { moduleOrigin = "String"
                                                                                                    , qualification = "String"
                                                                                                    , name = "dropLeft"
                                                                                                    }
                                                                                            }
                                                                                        , argument0 =
                                                                                            { range = Elm.Syntax.Range.empty
                                                                                            , type_ = typeInt
                                                                                            , value =
                                                                                                ElmSyntaxTypeInfer.ExpressionInteger
                                                                                                    { base = ElmSyntaxTypeInfer.Base10
                                                                                                    , value = 4
                                                                                                    }
                                                                                            }
                                                                                        , argument1Up = []
                                                                                        }
                                                                                }
                                                                            }
                                                                    }
                                                                }
                                                        }
                                                    , argument1Up =
                                                        [ { range = Elm.Syntax.Range.empty
                                                          , type_ = typeList typeString
                                                          , value =
                                                                ElmSyntaxTypeInfer.ExpressionReference
                                                                    { moduleOrigin = ""
                                                                    , qualification = ""
                                                                    , name = "lines"
                                                                    }
                                                          }
                                                        ]
                                                    }
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "inner types are consistent in unindent : List String -> List String ; unindent lines = lines |> List.map identity"
            (\() ->
                """module A exposing (..)
unindent : List String -> List String
unindent lines = lines |> List.map identity
"""
                    |> typeInferModuleFromSource
                    |> Result.map
                        (List.map
                            (\inferred ->
                                { result = inferred.result
                                , parameters = inferred.parameters
                                }
                            )
                        )
                    |> Expect.equal
                        (Ok
                            [ { parameters =
                                    [ { range = { end = { column = 15, row = 3 }, start = { column = 10, row = 3 } }
                                      , type_ = typeList typeString
                                      , value =
                                            ElmSyntaxTypeInfer.PatternVariable "lines"
                                      }
                                    ]
                              , result =
                                    { range = { end = { column = 44, row = 3 }, start = { column = 18, row = 3 } }
                                    , type_ = typeList typeString
                                    , value =
                                        ElmSyntaxTypeInfer.ExpressionInfixOperation
                                            { left =
                                                { range = { end = { column = 23, row = 3 }, start = { column = 18, row = 3 } }
                                                , type_ = typeList typeString
                                                , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "", name = "lines", qualification = "" }
                                                }
                                            , operator =
                                                { moduleOrigin = "Basics"
                                                , symbol = "|>"
                                                , type_ =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                            { input = typeList typeString
                                                            , output =
                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                        { input =
                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                    { input = typeList typeString
                                                                                    , output = typeList typeString
                                                                                    }
                                                                                )
                                                                        , output = typeList typeString
                                                                        }
                                                                    )
                                                            }
                                                        )
                                                }
                                            , right =
                                                { range = { end = { column = 44, row = 3 }, start = { column = 27, row = 3 } }
                                                , type_ =
                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                            { input = typeList typeString
                                                            , output = typeList typeString
                                                            }
                                                        )
                                                , value =
                                                    ElmSyntaxTypeInfer.ExpressionCall
                                                        { argument0 =
                                                            { range = { end = { column = 44, row = 3 }, start = { column = 36, row = 3 } }
                                                            , type_ =
                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                        { input = typeString
                                                                        , output = typeString
                                                                        }
                                                                    )
                                                            , value =
                                                                ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "Basics", name = "identity", qualification = "" }
                                                            }
                                                        , argument1Up = []
                                                        , called =
                                                            { range = { end = { column = 35, row = 3 }, start = { column = 27, row = 3 } }
                                                            , type_ =
                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                        { input =
                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                    { input = typeString
                                                                                    , output = typeString
                                                                                    }
                                                                                )
                                                                        , output =
                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                    { input = typeList typeString
                                                                                    , output = typeList typeString
                                                                                    }
                                                                                )
                                                                        }
                                                                    )
                                                            , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "List", name = "map", qualification = "List" }
                                                            }
                                                        }
                                                }
                                            }
                                    }
                              }
                            ]
                        )
            )
        , Test.test "inner types are consistent in unindent : List String -> List String ; unindent lines = lines |> List.map (\\line -> line)"
            (\() ->
                """module A exposing (..)
unindent : List String -> List String
unindent lines = lines |> List.map (\\line -> line)
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = { end = { column = 51, row = 3 }, start = { column = 18, row = 3 } }
                                , type_ = typeList typeString
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionInfixOperation
                                        { left =
                                            { range = { end = { column = 23, row = 3 }, start = { column = 18, row = 3 } }
                                            , type_ = typeList typeString
                                            , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "", name = "lines", qualification = "" }
                                            }
                                        , operator =
                                            { moduleOrigin = "Basics"
                                            , symbol = "|>"
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input = typeList typeString
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeList typeString
                                                                                , output = typeList typeString
                                                                                }
                                                                            )
                                                                    , output = typeList typeString
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        , right =
                                            { range = { end = { column = 51, row = 3 }, start = { column = 27, row = 3 } }
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input = typeList typeString
                                                        , output = typeList typeString
                                                        }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionCall
                                                    { argument0 =
                                                        { range = { end = { column = 51, row = 3 }, start = { column = 36, row = 3 } }
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeString
                                                                    , output = typeString
                                                                    }
                                                                )
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionParenthesized
                                                                { range = { end = { column = 50, row = 3 }, start = { column = 37, row = 3 } }
                                                                , type_ =
                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                            { input = typeString
                                                                            , output = typeString
                                                                            }
                                                                        )
                                                                , value =
                                                                    ElmSyntaxTypeInfer.ExpressionLambda
                                                                        { parameter0 =
                                                                            { range = { end = { column = 42, row = 3 }, start = { column = 38, row = 3 } }
                                                                            , type_ = typeString
                                                                            , value = ElmSyntaxTypeInfer.PatternVariable "line"
                                                                            }
                                                                        , parameter1Up = []
                                                                        , result =
                                                                            { range = { end = { column = 50, row = 3 }, start = { column = 46, row = 3 } }
                                                                            , type_ = typeString
                                                                            , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "", name = "line", qualification = "" }
                                                                            }
                                                                        }
                                                                }
                                                        }
                                                    , argument1Up = []
                                                    , called =
                                                        { range = { end = { column = 35, row = 3 }, start = { column = 27, row = 3 } }
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeString
                                                                                , output = typeString
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeList typeString
                                                                                , output = typeList typeString
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "List", name = "map", qualification = "List" }
                                                        }
                                                    }
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "inner types are consistent in unindent : List String -> List String ; unindent lines = lines |> List.map (\\line -> line |> String.dropLeft 4)"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "unindent"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                    )
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "unindent"
                        , arguments =
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "lines")
                            ]
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.OperatorApplication
                                    "|>"
                                    Elm.Syntax.Infix.Left
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "lines")
                                    )
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.Application
                                            [ Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                                            , Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.LambdaExpression
                                                    { args =
                                                        [ Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Pattern.VarPattern "line")
                                                        ]
                                                    , expression =
                                                        Elm.Syntax.Node.empty
                                                            (Elm.Syntax.Expression.OperatorApplication
                                                                "|>"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.FunctionOrValue [] "line")
                                                                )
                                                                (Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.Application
                                                                        [ Elm.Syntax.Node.empty
                                                                            (Elm.Syntax.Expression.FunctionOrValue [ "String" ] "dropLeft")
                                                                        , Elm.Syntax.Node.empty
                                                                            (Elm.Syntax.Expression.Integer 4)
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                    }
                                                )
                                            ]
                                        )
                                    )
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ = typeList typeString
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionInfixOperation
                                        { operator =
                                            { moduleOrigin = "Basics"
                                            , symbol = "|>"
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input = typeList typeString
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeList typeString
                                                                                , output = typeList typeString
                                                                                }
                                                                            )
                                                                    , output = typeList typeString
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        , left =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ = typeList typeString
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReference
                                                    { moduleOrigin = ""
                                                    , qualification = ""
                                                    , name = "lines"
                                                    }
                                            }
                                        , right =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input = typeList typeString
                                                        , output = typeList typeString
                                                        }
                                                    )
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionCall
                                                    { called =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeString
                                                                                , output = typeString
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeList typeString
                                                                                , output = typeList typeString
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionReference
                                                                { moduleOrigin = "List"
                                                                , qualification = "List"
                                                                , name = "map"
                                                                }
                                                        }
                                                    , argument0 =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeString, output = typeString }
                                                                )
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionLambda
                                                                { parameter0 =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ = typeString
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.PatternVariable "line"
                                                                    }
                                                                , parameter1Up = []
                                                                , result =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ = typeString
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionInfixOperation
                                                                            { operator =
                                                                                { moduleOrigin = "Basics"
                                                                                , symbol = "|>"
                                                                                , type_ =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeString
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input = typeString
                                                                                                                    , output = typeString
                                                                                                                    }
                                                                                                                )
                                                                                                        , output = typeString
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            , left =
                                                                                { range = Elm.Syntax.Range.empty
                                                                                , type_ = typeString
                                                                                , value =
                                                                                    ElmSyntaxTypeInfer.ExpressionReference
                                                                                        { moduleOrigin = ""
                                                                                        , qualification = ""
                                                                                        , name = "line"
                                                                                        }
                                                                                }
                                                                            , right =
                                                                                { range = Elm.Syntax.Range.empty
                                                                                , type_ =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeString
                                                                                            , output = typeString
                                                                                            }
                                                                                        )
                                                                                , value =
                                                                                    ElmSyntaxTypeInfer.ExpressionCall
                                                                                        { called =
                                                                                            { range = Elm.Syntax.Range.empty
                                                                                            , type_ =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input = typeInt
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input = typeString
                                                                                                                    , output = typeString
                                                                                                                    }
                                                                                                                )
                                                                                                        }
                                                                                                    )
                                                                                            , value =
                                                                                                ElmSyntaxTypeInfer.ExpressionReference
                                                                                                    { moduleOrigin = "String"
                                                                                                    , qualification = "String"
                                                                                                    , name = "dropLeft"
                                                                                                    }
                                                                                            }
                                                                                        , argument0 =
                                                                                            { range = Elm.Syntax.Range.empty
                                                                                            , type_ = typeInt
                                                                                            , value =
                                                                                                ElmSyntaxTypeInfer.ExpressionInteger
                                                                                                    { base = ElmSyntaxTypeInfer.Base10
                                                                                                    , value = 4
                                                                                                    }
                                                                                            }
                                                                                        , argument1Up = []
                                                                                        }
                                                                                }
                                                                            }
                                                                    }
                                                                }
                                                        }
                                                    , argument1Up = []
                                                    }
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "inner types are consistent in unindent : List String -> List String ; unindent lines = if True then lines else lines |> List.map (\\line -> line |> String.dropLeft 4)"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "unindent"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.empty ( [], "List" ))
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "String" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                    )
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "unindent"
                        , arguments =
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "lines")
                            ]
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.IfBlock
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "True")
                                    )
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.FunctionOrValue [] "lines")
                                    )
                                    (Elm.Syntax.Node.empty
                                        (Elm.Syntax.Expression.OperatorApplication
                                            "|>"
                                            Elm.Syntax.Infix.Left
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.FunctionOrValue [] "lines")
                                            )
                                            (Elm.Syntax.Node.empty
                                                (Elm.Syntax.Expression.Application
                                                    [ Elm.Syntax.Node.empty
                                                        (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                                                    , Elm.Syntax.Node.empty
                                                        (Elm.Syntax.Expression.LambdaExpression
                                                            { args =
                                                                [ Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Pattern.VarPattern "line")
                                                                ]
                                                            , expression =
                                                                Elm.Syntax.Node.empty
                                                                    (Elm.Syntax.Expression.OperatorApplication
                                                                        "|>"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.empty
                                                                            (Elm.Syntax.Expression.FunctionOrValue [] "line")
                                                                        )
                                                                        (Elm.Syntax.Node.empty
                                                                            (Elm.Syntax.Expression.Application
                                                                                [ Elm.Syntax.Node.empty
                                                                                    (Elm.Syntax.Expression.FunctionOrValue [ "String" ] "dropLeft")
                                                                                , Elm.Syntax.Node.empty
                                                                                    (Elm.Syntax.Expression.Integer 4)
                                                                                ]
                                                                            )
                                                                        )
                                                                    )
                                                            }
                                                        )
                                                    ]
                                                )
                                            )
                                        )
                                    )
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ = typeList typeString
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionIfThenElse
                                        { condition =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ = typeBool
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReferenceVariant
                                                    { moduleOrigin = "Basics"
                                                    , choiceTypeName = "Bool"
                                                    , name = "True"
                                                    , qualification = ""
                                                    }
                                            }
                                        , onTrue =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ = typeList typeString
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionReference
                                                    { moduleOrigin = ""
                                                    , name = "lines"
                                                    , qualification = ""
                                                    }
                                            }
                                        , onFalse =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ = typeList typeString
                                            , value =
                                                ElmSyntaxTypeInfer.ExpressionInfixOperation
                                                    { operator =
                                                        { moduleOrigin = "Basics"
                                                        , symbol = "|>"
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeList typeString
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeList typeString
                                                                                            , output = typeList typeString
                                                                                            }
                                                                                        )
                                                                                , output = typeList typeString
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    , left =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ = typeList typeString
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionReference
                                                                { moduleOrigin = ""
                                                                , qualification = ""
                                                                , name = "lines"
                                                                }
                                                        }
                                                    , right =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input = typeList typeString
                                                                    , output = typeList typeString
                                                                    }
                                                                )
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionCall
                                                                { called =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeString
                                                                                            , output = typeString
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input = typeList typeString
                                                                                            , output = typeList typeString
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionReference
                                                                            { moduleOrigin = "List"
                                                                            , qualification = "List"
                                                                            , name = "map"
                                                                            }
                                                                    }
                                                                , argument0 =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input = typeString, output = typeString }
                                                                            )
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionLambda
                                                                            { parameter0 =
                                                                                { range = Elm.Syntax.Range.empty
                                                                                , type_ = typeString
                                                                                , value =
                                                                                    ElmSyntaxTypeInfer.PatternVariable "line"
                                                                                }
                                                                            , parameter1Up = []
                                                                            , result =
                                                                                { range = Elm.Syntax.Range.empty
                                                                                , type_ = typeString
                                                                                , value =
                                                                                    ElmSyntaxTypeInfer.ExpressionInfixOperation
                                                                                        { operator =
                                                                                            { moduleOrigin = "Basics"
                                                                                            , symbol = "|>"
                                                                                            , type_ =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input = typeString
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input =
                                                                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                                { input = typeString
                                                                                                                                , output = typeString
                                                                                                                                }
                                                                                                                            )
                                                                                                                    , output = typeString
                                                                                                                    }
                                                                                                                )
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        , left =
                                                                                            { range = Elm.Syntax.Range.empty
                                                                                            , type_ = typeString
                                                                                            , value =
                                                                                                ElmSyntaxTypeInfer.ExpressionReference
                                                                                                    { moduleOrigin = ""
                                                                                                    , qualification = ""
                                                                                                    , name = "line"
                                                                                                    }
                                                                                            }
                                                                                        , right =
                                                                                            { range = Elm.Syntax.Range.empty
                                                                                            , type_ =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input = typeString
                                                                                                        , output = typeString
                                                                                                        }
                                                                                                    )
                                                                                            , value =
                                                                                                ElmSyntaxTypeInfer.ExpressionCall
                                                                                                    { called =
                                                                                                        { range = Elm.Syntax.Range.empty
                                                                                                        , type_ =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input = typeInt
                                                                                                                    , output =
                                                                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                                { input = typeString
                                                                                                                                , output = typeString
                                                                                                                                }
                                                                                                                            )
                                                                                                                    }
                                                                                                                )
                                                                                                        , value =
                                                                                                            ElmSyntaxTypeInfer.ExpressionReference
                                                                                                                { moduleOrigin = "String"
                                                                                                                , qualification = "String"
                                                                                                                , name = "dropLeft"
                                                                                                                }
                                                                                                        }
                                                                                                    , argument0 =
                                                                                                        { range = Elm.Syntax.Range.empty
                                                                                                        , type_ = typeInt
                                                                                                        , value =
                                                                                                            ElmSyntaxTypeInfer.ExpressionInteger
                                                                                                                { base = ElmSyntaxTypeInfer.Base10
                                                                                                                , value = 4
                                                                                                                }
                                                                                                        }
                                                                                                    , argument1Up = []
                                                                                                    }
                                                                                            }
                                                                                        }
                                                                                }
                                                                            }
                                                                    }
                                                                , argument1Up = []
                                                                }
                                                        }
                                                    }
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "single un-annotated let declaration as call result: tupleFirstInt : ( Int, Int ) -> () ; tupleFirstInt tuple = let firstInt = Tuple.first tuple in ()"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "tupleFirstInt"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.empty
                                            (Elm.Syntax.TypeAnnotation.Tupled
                                                [ Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "Int" ))
                                                        []
                                                    )
                                                , Elm.Syntax.Node.empty
                                                    (Elm.Syntax.TypeAnnotation.Typed
                                                        (Elm.Syntax.Node.empty ( [], "Int" ))
                                                        []
                                                    )
                                                ]
                                            )
                                        )
                                        (Elm.Syntax.Node.empty
                                            Elm.Syntax.TypeAnnotation.Unit
                                        )
                                    )
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "tupleFirstInt"
                        , arguments =
                            [ Elm.Syntax.Node.empty
                                (Elm.Syntax.Pattern.VarPattern "tuple")
                            ]
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.LetExpression
                                    { declarations =
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    Elm.Syntax.Node.empty
                                                        { name = Elm.Syntax.Node.empty "firstInt"
                                                        , arguments = []
                                                        , expression =
                                                            Elm.Syntax.Node.empty
                                                                (Elm.Syntax.Expression.Application
                                                                    [ Elm.Syntax.Node.empty
                                                                        (Elm.Syntax.Expression.FunctionOrValue [ "Tuple" ] "first")
                                                                    , Elm.Syntax.Node.empty
                                                                        (Elm.Syntax.Expression.FunctionOrValue [] "tuple")
                                                                    ]
                                                                )
                                                        }
                                                }
                                            )
                                        ]
                                    , expression =
                                        Elm.Syntax.Node.empty
                                            Elm.Syntax.Expression.UnitExpr
                                    }
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        ElmSyntaxTypeInfer.TypeUnit
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionLetIn
                                        { declaration0 =
                                            { range = Elm.Syntax.Range.empty
                                            , declaration =
                                                ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                                                    { signature = Nothing
                                                    , nameRange = Elm.Syntax.Range.empty
                                                    , name = "firstInt"
                                                    , parameters = []
                                                    , result =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ = typeInt
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionCall
                                                                { called =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeTuple
                                                                                            { part0 = typeInt, part1 = typeInt }
                                                                                        )
                                                                                , output = typeInt
                                                                                }
                                                                            )
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionReference
                                                                            { qualification = "Tuple"
                                                                            , moduleOrigin = "Tuple"
                                                                            , name = "first"
                                                                            }
                                                                    }
                                                                , argument0 =
                                                                    { range = Elm.Syntax.Range.empty
                                                                    , type_ =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeTuple
                                                                                { part0 = typeInt, part1 = typeInt }
                                                                            )
                                                                    , value =
                                                                        ElmSyntaxTypeInfer.ExpressionReference
                                                                            { qualification = ""
                                                                            , moduleOrigin = ""
                                                                            , name = "tuple"
                                                                            }
                                                                    }
                                                                , argument1Up = []
                                                                }
                                                        }
                                                    , type_ = typeInt
                                                    }
                                            }
                                        , declaration1Up = []
                                        , result =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    ElmSyntaxTypeInfer.TypeUnit
                                            , value = ElmSyntaxTypeInfer.ExpressionUnit
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "single un-annotated let declaration with constrained variable type as call result: let n = 2 in ()"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "waste"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    Elm.Syntax.TypeAnnotation.Unit
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "waste"
                        , arguments = []
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.LetExpression
                                    { declarations =
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    Elm.Syntax.Node.empty
                                                        { name = Elm.Syntax.Node.empty "n"
                                                        , arguments = []
                                                        , expression =
                                                            Elm.Syntax.Node.empty
                                                                (Elm.Syntax.Expression.Integer 2)
                                                        }
                                                }
                                            )
                                        ]
                                    , expression =
                                        Elm.Syntax.Node.empty
                                            Elm.Syntax.Expression.UnitExpr
                                    }
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        ElmSyntaxTypeInfer.TypeUnit
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionLetIn
                                        { declaration0 =
                                            { range = Elm.Syntax.Range.empty
                                            , declaration =
                                                ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                                                    { signature = Nothing
                                                    , nameRange = Elm.Syntax.Range.empty
                                                    , name = "n"
                                                    , parameters = []
                                                    , result =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                { useRange = Elm.Syntax.Range.empty
                                                                , name = "number"
                                                                }
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionInteger
                                                                { base = ElmSyntaxTypeInfer.Base10
                                                                , value = 2
                                                                }
                                                        }
                                                    , type_ =
                                                        ElmSyntaxTypeInfer.TypeVariable
                                                            { useRange = Elm.Syntax.Range.empty
                                                            , name = "number"
                                                            }
                                                    }
                                            }
                                        , declaration1Up = []
                                        , result =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    ElmSyntaxTypeInfer.TypeUnit
                                            , value = ElmSyntaxTypeInfer.ExpressionUnit
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "transitive un-annotated let declarations with constrained variable type as call result: let a = 2 ; b = a in ()"
            (\() ->
                """module A exposing (..)
waste =
    let
        a = 2 
        b = a
    in
    ()
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = { end = { column = 7, row = 7 }, start = { column = 5, row = 3 } }
                                , type_ = ElmSyntaxTypeInfer.TypeNotVariable ElmSyntaxTypeInfer.TypeUnit
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionLetIn
                                        { declaration0 =
                                            { declaration =
                                                ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                                                    { name = "a"
                                                    , nameRange = { end = { column = 10, row = 4 }, start = { column = 9, row = 4 } }
                                                    , parameters = []
                                                    , result =
                                                        { range = { end = { column = 14, row = 4 }, start = { column = 13, row = 4 } }
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                { useRange = { end = { column = 14, row = 4 }, start = { column = 13, row = 4 } }
                                                                , name = "number"
                                                                }
                                                        , value = ElmSyntaxTypeInfer.ExpressionInteger { base = ElmSyntaxTypeInfer.Base10, value = 2 }
                                                        }
                                                    , signature = Nothing
                                                    , type_ =
                                                        ElmSyntaxTypeInfer.TypeVariable
                                                            { useRange = { end = { column = 14, row = 4 }, start = { column = 13, row = 4 } }
                                                            , name = "number"
                                                            }
                                                    }
                                            , range = { end = { column = 14, row = 4 }, start = { column = 9, row = 4 } }
                                            }
                                        , declaration1Up =
                                            [ { declaration =
                                                    ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                                                        { name = "b"
                                                        , nameRange =
                                                            { end = { column = 10, row = 5 }
                                                            , start = { column = 9, row = 5 }
                                                            }
                                                        , parameters = []
                                                        , result =
                                                            { range =
                                                                { end = { column = 14, row = 5 }
                                                                , start = { column = 13, row = 5 }
                                                                }
                                                            , type_ =
                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                    { useRange = { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    , name = "number1"
                                                                    }
                                                            , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "", name = "a", qualification = "" }
                                                            }
                                                        , signature = Nothing
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                { useRange = { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                , name = "number1"
                                                                }
                                                        }
                                              , range =
                                                    { end = { column = 14, row = 5 }
                                                    , start = { column = 9, row = 5 }
                                                    }
                                              }
                                            ]
                                        , result =
                                            { range =
                                                { end = { column = 7, row = 7 }
                                                , start = { column = 5, row = 7 }
                                                }
                                            , type_ = ElmSyntaxTypeInfer.TypeNotVariable ElmSyntaxTypeInfer.TypeUnit
                                            , value = ElmSyntaxTypeInfer.ExpressionUnit
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "transitive (with a unification) un-annotated let declarations involving constrained variable type: let a = 2 ; b = -a in ()"
            (\() ->
                """module A exposing (..)
waste =
    let
        a = 2
        b = -a
    in
    ()
"""
                    |> typeInferModuleFromSource
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = { end = { column = 7, row = 7 }, start = { column = 5, row = 3 } }
                                , type_ = ElmSyntaxTypeInfer.TypeNotVariable ElmSyntaxTypeInfer.TypeUnit
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionLetIn
                                        { declaration0 =
                                            { declaration =
                                                ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                                                    { name = "a"
                                                    , nameRange = { end = { column = 10, row = 4 }, start = { column = 9, row = 4 } }
                                                    , parameters = []
                                                    , result =
                                                        { range = { end = { column = 14, row = 4 }, start = { column = 13, row = 4 } }
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                { useRange = { end = { column = 14, row = 4 }, start = { column = 13, row = 4 } }
                                                                , name = "number"
                                                                }
                                                        , value = ElmSyntaxTypeInfer.ExpressionInteger { base = ElmSyntaxTypeInfer.Base10, value = 2 }
                                                        }
                                                    , signature = Nothing
                                                    , type_ =
                                                        ElmSyntaxTypeInfer.TypeVariable
                                                            { useRange = { end = { column = 14, row = 4 }, start = { column = 13, row = 4 } }
                                                            , name = "number"
                                                            }
                                                    }
                                            , range = { end = { column = 14, row = 4 }, start = { column = 9, row = 4 } }
                                            }
                                        , declaration1Up =
                                            [ { declaration =
                                                    ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                                                        { name = "b"
                                                        , nameRange = { end = { column = 10, row = 5 }, start = { column = 9, row = 5 } }
                                                        , parameters = []
                                                        , result =
                                                            { range = { end = { column = 15, row = 5 }, start = { column = 13, row = 5 } }
                                                            , type_ =
                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                    { useRange = { end = { column = 15, row = 5 }, start = { column = 13, row = 5 } }
                                                                    , name = "number1"
                                                                    }
                                                            , value =
                                                                ElmSyntaxTypeInfer.ExpressionNegation
                                                                    { range = { end = { column = 15, row = 5 }, start = { column = 14, row = 5 } }
                                                                    , type_ =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            { useRange = { end = { column = 15, row = 5 }, start = { column = 13, row = 5 } }
                                                                            , name = "number1"
                                                                            }
                                                                    , value = ElmSyntaxTypeInfer.ExpressionReference { moduleOrigin = "", name = "a", qualification = "" }
                                                                    }
                                                            }
                                                        , signature = Nothing
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                { useRange = { end = { column = 15, row = 5 }, start = { column = 13, row = 5 } }
                                                                , name = "number1"
                                                                }
                                                        }
                                              , range = { end = { column = 15, row = 5 }, start = { column = 9, row = 5 } }
                                              }
                                            ]
                                        , result =
                                            { range = { end = { column = 7, row = 7 }, start = { column = 5, row = 7 } }
                                            , type_ = ElmSyntaxTypeInfer.TypeNotVariable ElmSyntaxTypeInfer.TypeUnit
                                            , value = ElmSyntaxTypeInfer.ExpressionUnit
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "transitive (with a unification to concrete type) un-annotated let declarations involving constrained variable type: let a = 2 ; b = round a in ()"
            (\() ->
                { documentation = Nothing
                , signature =
                    Just
                        (Elm.Syntax.Node.empty
                            { name = Elm.Syntax.Node.empty "waste"
                            , typeAnnotation =
                                Elm.Syntax.Node.empty
                                    Elm.Syntax.TypeAnnotation.Unit
                            }
                        )
                , declaration =
                    Elm.Syntax.Node.empty
                        { name = Elm.Syntax.Node.empty "waste"
                        , arguments = []
                        , expression =
                            Elm.Syntax.Node.empty
                                (Elm.Syntax.Expression.LetExpression
                                    { declarations =
                                        [ Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    Elm.Syntax.Node.empty
                                                        { name =
                                                            Elm.Syntax.Node.Node
                                                                { start = { row = 1, column = 0 }
                                                                , end = { row = 1, column = 0 }
                                                                }
                                                                "a"
                                                        , arguments = []
                                                        , expression =
                                                            Elm.Syntax.Node.empty
                                                                (Elm.Syntax.Expression.Integer 2)
                                                        }
                                                }
                                            )
                                        , Elm.Syntax.Node.empty
                                            (Elm.Syntax.Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    Elm.Syntax.Node.empty
                                                        { name =
                                                            Elm.Syntax.Node.Node
                                                                { start = { row = 2, column = 0 }
                                                                , end = { row = 2, column = 0 }
                                                                }
                                                                "b"
                                                        , arguments = []
                                                        , expression =
                                                            Elm.Syntax.Node.empty
                                                                (Elm.Syntax.Expression.Application
                                                                    [ Elm.Syntax.Node.empty
                                                                        (Elm.Syntax.Expression.FunctionOrValue [] "round")
                                                                    , Elm.Syntax.Node.Node
                                                                        { start = { row = 3, column = 0 }
                                                                        , end = { row = 3, column = 0 }
                                                                        }
                                                                        (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                                                    ]
                                                                )
                                                        }
                                                }
                                            )
                                        ]
                                    , expression =
                                        Elm.Syntax.Node.empty
                                            Elm.Syntax.Expression.UnitExpr
                                    }
                                )
                        }
                }
                    |> List.singleton
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = "A"
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = exampleModuleOriginLookup
                        , otherModuleDeclaredTypes =
                            []
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = "A"
                                    , moduleOriginLookup = exampleModuleOriginLookup
                                    }
                                |> .types
                        }
                    |> Result.map (List.map .result)
                    |> Expect.equal
                        (Ok
                            (List.singleton
                                { range = Elm.Syntax.Range.empty
                                , type_ =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        ElmSyntaxTypeInfer.TypeUnit
                                , value =
                                    ElmSyntaxTypeInfer.ExpressionLetIn
                                        { declaration0 =
                                            { range = Elm.Syntax.Range.empty
                                            , declaration =
                                                ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                                                    { signature = Nothing
                                                    , nameRange =
                                                        { start = { row = 1, column = 0 }
                                                        , end = { row = 1, column = 0 }
                                                        }
                                                    , name = "a"
                                                    , parameters = []
                                                    , result =
                                                        { range = Elm.Syntax.Range.empty
                                                        , type_ =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                { useRange = Elm.Syntax.Range.empty
                                                                , name = "number"
                                                                }
                                                        , value =
                                                            ElmSyntaxTypeInfer.ExpressionInteger
                                                                { base = ElmSyntaxTypeInfer.Base10
                                                                , value = 2
                                                                }
                                                        }
                                                    , type_ =
                                                        ElmSyntaxTypeInfer.TypeVariable
                                                            { useRange = Elm.Syntax.Range.empty
                                                            , name = "number"
                                                            }
                                                    }
                                            }
                                        , declaration1Up =
                                            [ { range = Elm.Syntax.Range.empty
                                              , declaration =
                                                    ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                                                        { signature = Nothing
                                                        , nameRange =
                                                            { start = { row = 2, column = 0 }
                                                            , end = { row = 2, column = 0 }
                                                            }
                                                        , name = "b"
                                                        , parameters = []
                                                        , result =
                                                            { range = Elm.Syntax.Range.empty
                                                            , type_ = typeInt
                                                            , value =
                                                                ElmSyntaxTypeInfer.ExpressionCall
                                                                    { called =
                                                                        { range = Elm.Syntax.Range.empty
                                                                        , type_ =
                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                    { input = typeFloat, output = typeInt }
                                                                                )
                                                                        , value =
                                                                            ElmSyntaxTypeInfer.ExpressionReference
                                                                                { moduleOrigin = "Basics"
                                                                                , qualification = ""
                                                                                , name = "round"
                                                                                }
                                                                        }
                                                                    , argument0 =
                                                                        { range =
                                                                            { start = { row = 3, column = 0 }
                                                                            , end = { row = 3, column = 0 }
                                                                            }
                                                                        , type_ = typeFloat
                                                                        , value =
                                                                            ElmSyntaxTypeInfer.ExpressionReference
                                                                                { moduleOrigin = ""
                                                                                , qualification = ""
                                                                                , name = "a"
                                                                                }
                                                                        }
                                                                    , argument1Up = []
                                                                    }
                                                            }
                                                        , type_ = typeInt
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = Elm.Syntax.Range.empty
                                            , type_ =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    ElmSyntaxTypeInfer.TypeUnit
                                            , value = ElmSyntaxTypeInfer.ExpressionUnit
                                            }
                                        }
                                }
                            )
                        )
            )
        , Test.test "type variables are preserved across let use when variable comes from lambda: \\a -> let b = -a in b"
            (\() ->
                """module A exposing (..)
waste =
    \\a -> let b = -a in b
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 26, row = 3 }, start = { column = 6, row = 3 } }
                                            , name = "number"
                                            }
                                    , output =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 26, row = 3 }, start = { column = 6, row = 3 } }
                                            , name = "number"
                                            }
                                    }
                                )
                            )
                        )
            )
        , Test.test "type variables are preserved across let use when variable comes from declaration parameter: (parameter) a = let b = -a in b"
            (\() ->
                """module A exposing (..)
waste a =
    let b = -a in b
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 20, row = 3 }, start = { column = 7, row = 2 } }
                                            , name = "number"
                                            }
                                    , output =
                                        ElmSyntaxTypeInfer.TypeVariable
                                            { useRange = { end = { column = 20, row = 3 }, start = { column = 7, row = 2 } }
                                            , name = "number"
                                            }
                                    }
                                )
                            )
                        )
            )
        , Test.test "import alias"
            (\() ->
                """module A exposing (..)
import Array as Arr
waste =
    Arr.empty
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Array"
                                    , name = "Array"
                                    , arguments =
                                        [ ElmSyntaxTypeInfer.TypeVariable
                                            { name = "a"
                                            , useRange = { end = { column = 14, row = 4 }, start = { column = 5, row = 4 } }
                                            }
                                        ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "import alias equal to actual module name of explicit import of by default implicitly imported module"
            (\() ->
                """module A exposing (..)
import String
import Char as String
waste =
    String.isAlpha
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeFunction
                                    { input = typeChar
                                    , output = typeBool
                                    }
                                )
                            )
                        )
            )
        , Test.test "import alias equal to actual module name"
            (\() ->
                """module A exposing (..)
import Array
import List as Array
waste =
    Array.fromList [ "" ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Array"
                                    , name = "Array"
                                    , arguments = [ typeString ]
                                    }
                                )
                            )
                        )
            )
        , Test.test "import alias equal to import alias of a different import"
            (\() ->
                """module A exposing (..)
import Set as Array
import List as Array
waste =
    Array.fromList [ "" ]
"""
                    |> typeInferModuleFromSource
                    |> Result.andThen toSingleInferredDeclaration
                    |> Expect.equal
                        (Ok
                            (ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = "Set"
                                    , name = "Set"
                                    , arguments = [ typeString ]
                                    }
                                )
                            )
                        )
            )
        ]


typeList : ElmSyntaxTypeInfer.Type -> ElmSyntaxTypeInfer.Type
typeList element =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = "List"
            , name = "List"
            , arguments = [ element ]
            }
        )


typeMaybe : ElmSyntaxTypeInfer.Type -> ElmSyntaxTypeInfer.Type
typeMaybe value =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = "Maybe"
            , name = "Maybe"
            , arguments = [ value ]
            }
        )


typeFloat : ElmSyntaxTypeInfer.Type
typeFloat =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = "Basics"
            , name = "Float"
            , arguments = []
            }
        )


typeInt : ElmSyntaxTypeInfer.Type
typeInt =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = "Basics"
            , name = "Int"
            , arguments = []
            }
        )


typeBool : ElmSyntaxTypeInfer.Type
typeBool =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = "Basics"
            , name = "Bool"
            , arguments = []
            }
        )


typeOrder : ElmSyntaxTypeInfer.Type
typeOrder =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = "Basics"
            , name = "Order"
            , arguments = []
            }
        )


typeString : ElmSyntaxTypeInfer.Type
typeString =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = "String"
            , name = "String"
            , arguments = []
            }
        )


typeChar : ElmSyntaxTypeInfer.Type
typeChar =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = "Char"
            , name = "Char"
            , arguments = []
            }
        )


expressionExpectInferredType :
    ElmSyntaxTypeInfer.Type
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
    -> Result String ElmSyntaxTypeInfer.Type
expressionToInferredType expression =
    [ expressionWrapInExampleDeclaration expression ]
        |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
            { moduleName = "A"
            , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
            , moduleOriginLookup = exampleModuleOriginLookup
            , otherModuleDeclaredTypes =
                []
                    |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                        { moduleName = "A"
                        , moduleOriginLookup = exampleModuleOriginLookup
                        }
                    |> .types
            }
        |> Debug.log "actual"
        |> Result.andThen toSingleInferredDeclaration


toSingleInferredDeclaration : List { inferred_ | type_ : type_ } -> Result String type_
toSingleInferredDeclaration declarationsInferred =
    case declarationsInferred |> List.head of
        Nothing ->
            Err "no typed declarations found"

        Just declarationTyped ->
            Ok declarationTyped.type_


exampleModuleOriginLookupImportingDict : ElmSyntaxTypeInfer.ModuleOriginLookup
exampleModuleOriginLookupImportingDict =
    [ Elm.Syntax.Node.empty
        { moduleName = Elm.Syntax.Node.empty [ "Dict" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    ]
        |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
            ElmSyntaxTypeInfer.elmCoreTypes


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


typeInferModuleFromSource :
    String
    ->
        Result
            String
            (List
                { name : String
                , parameters :
                    List (ElmSyntaxTypeInfer.TypedNode ElmSyntaxTypeInfer.Pattern)
                , result :
                    ElmSyntaxTypeInfer.TypedNode ElmSyntaxTypeInfer.Expression
                , type_ : ElmSyntaxTypeInfer.Type
                , nameRange : Elm.Syntax.Range.Range
                , documentation : Maybe { content : String, range : Elm.Syntax.Range.Range }
                , signature :
                    Maybe
                        { range : Elm.Syntax.Range.Range
                        , nameRange : Elm.Syntax.Range.Range
                        , annotationTypeRange : Elm.Syntax.Range.Range
                        , annotationType : Elm.Syntax.TypeAnnotation.TypeAnnotation
                        }
                }
            )
typeInferModuleFromSource moduleSource =
    moduleSource
        |> Elm.Parser.parseToFile
        |> Result.mapError (\_ -> "failed to parse")
        |> Result.andThen
            (\parsed ->
                let
                    moduleOriginLookup : ElmSyntaxTypeInfer.ModuleOriginLookup
                    moduleOriginLookup =
                        parsed.imports
                            |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                                ElmSyntaxTypeInfer.elmCoreTypes

                    moduleName : String
                    moduleName =
                        parsed.moduleDefinition
                            |> Elm.Syntax.Node.value
                            |> moduleHeaderName
                in
                parsed.declarations
                    |> List.filterMap
                        (\(Elm.Syntax.Node.Node _ parsedDeclaration) ->
                            case parsedDeclaration of
                                Elm.Syntax.Declaration.FunctionDeclaration function ->
                                    Just function

                                Elm.Syntax.Declaration.AliasDeclaration _ ->
                                    Nothing

                                Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                                    Nothing

                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                    Nothing

                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                    Nothing

                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                    Nothing
                        )
                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                        { moduleName = moduleName
                        , importedTypes = ElmSyntaxTypeInfer.elmCoreTypes
                        , moduleOriginLookup = moduleOriginLookup
                        , otherModuleDeclaredTypes =
                            parsed.declarations
                                |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                    { moduleName = moduleName
                                    , moduleOriginLookup = moduleOriginLookup
                                    }
                                |> .types
                        }
            )


moduleHeaderName : Elm.Syntax.Module.Module -> String
moduleHeaderName moduleHeader =
    (case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName

        Elm.Syntax.Module.PortModule header ->
            header.moduleName

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName
    )
        |> Elm.Syntax.Node.value
        |> String.join "."
