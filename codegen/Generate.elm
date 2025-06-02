module Generate exposing (main)

import Elm
import Elm.Docs
import Elm.Syntax.ModuleName
import Elm.Type
import ElmSyntaxTypeInfer
import FastDict
import Gen.CodeGen.Generate
import Gen.ElmSyntaxTypeInfer
import Gen.FastDict
import Gen.List
import Gen.Maybe
import Json.Decode
import Json.Encode
import Elm.Syntax.Range


main : Program Json.Encode.Value () ()
main =
    Gen.CodeGen.Generate.fromJson
        (Json.Decode.list Elm.Docs.decoder)
        (\moduleInterfaces ->
            [ Elm.file [ "ElmCoreTypes" ]
                [ Elm.declaration "elmCoreTypes"
                    (Gen.FastDict.fromList
                        (moduleInterfaces
                            |> List.map moduleInterfaceAsTypesToExpression
                        )
                    )
                ]
            ]
        )


moduleInterfaceAsTypesToExpression : Elm.Docs.Module -> Elm.Expression
moduleInterfaceAsTypesToExpression moduleInterface =
    Elm.tuple
        (moduleNameToExpression (moduleInterface.name |> String.split "."))
        (Elm.record
            [ ( "signatures"
              , Gen.FastDict.fromList
                    (moduleInterface.values
                        |> List.map
                            (\valueOrFunctionInterface ->
                                Elm.tuple
                                    (Elm.string valueOrFunctionInterface.name)
                                    (case valueOrFunctionInterface.tipe |> interfaceToType of
                                        Err error ->
                                            Debug.todo error

                                        Ok type_ ->
                                            type_ |> typeToExpression
                                    )
                            )
                    )
              )
            , ( "typeAliases"
              , Gen.FastDict.fromList
                    (moduleInterface.aliases
                        |> List.map
                            (\typeAliasInterface ->
                                Elm.tuple
                                    (Elm.string typeAliasInterface.name)
                                    (Elm.record
                                        [ ( "parameters"
                                          , Elm.list (typeAliasInterface.args |> List.map Elm.string)
                                          )
                                        , ( "type_"
                                          , case typeAliasInterface.tipe |> interfaceToType of
                                                Err error ->
                                                    Debug.todo error

                                                Ok type_ ->
                                                    type_ |> typeToExpression
                                          )
                                        , ( "recordFieldOrder"
                                          , case typeAliasInterface.tipe of
                                                Elm.Type.Record fields Nothing ->
                                                    Gen.Maybe.make_.just
                                                        (Elm.list
                                                            (fields
                                                                |> List.map
                                                                    (\( name, _ ) ->
                                                                        Elm.string name
                                                                    )
                                                            )
                                                        )

                                                _ ->
                                                    Gen.Maybe.make_.nothing
                                          )
                                        ]
                                    )
                            )
                    )
              )
            , ( "choiceTypes"
              , if moduleInterface.name == "List" then
                    -- https://github.com/elm/core/issues/1037
                    Gen.FastDict.singleton (Elm.string "List")
                        (Elm.record
                            [ ( "parameters", Elm.list [ Elm.string "a" ] )
                            , ( "variants", Gen.FastDict.empty )
                            ]
                        )

                else
                    Gen.FastDict.fromList
                        (moduleInterface.unions
                            |> List.map
                                (\choiceTypeInterface ->
                                    Elm.tuple
                                        (Elm.string choiceTypeInterface.name)
                                        (Elm.record
                                            [ ( "parameters"
                                              , Elm.list (choiceTypeInterface.args |> List.map Elm.string)
                                              )
                                            , ( "variants"
                                              , Gen.FastDict.fromList
                                                    (choiceTypeInterface.tags
                                                        |> List.map
                                                            (\( variantName, variantValues ) ->
                                                                Elm.tuple
                                                                    (Elm.string variantName)
                                                                    (Elm.list
                                                                        (variantValues
                                                                            |> List.map
                                                                                (\variantValue ->
                                                                                    case variantValue |> interfaceToType of
                                                                                        Err error ->
                                                                                            Debug.todo error

                                                                                        Ok type_ ->
                                                                                            type_ |> typeToExpression
                                                                                )
                                                                        )
                                                                    )
                                                            )
                                                    )
                                              )
                                            ]
                                        )
                                )
                        )
              )
            ]
        )


typeToExpression : ElmSyntaxTypeInfer.Type -> Elm.Expression
typeToExpression type_ =
    case type_ of
        ElmSyntaxTypeInfer.TypeVariable variable ->
            Gen.ElmSyntaxTypeInfer.make_.typeVariable
                (Elm.record
                    [ ( "name", Elm.string variable.name )
                    , ( "useRange"
                      , Elm.value
                          { importFrom = [ "Elm", "Syntax", "Range" ]
                          , name = "empty"
                          , annotation = Nothing
                          }
                      )
                    ]
                )

        ElmSyntaxTypeInfer.TypeNotVariable typeNotVariable ->
            Gen.ElmSyntaxTypeInfer.make_.typeNotVariable
                (typeNotVariable |> typeNotVariableToExpression)


typeNotVariableToExpression : ElmSyntaxTypeInfer.TypeNotVariable -> Elm.Expression
typeNotVariableToExpression type_ =
    case type_ of
        ElmSyntaxTypeInfer.TypeUnit ->
            Gen.ElmSyntaxTypeInfer.make_.typeUnit

        ElmSyntaxTypeInfer.TypeConstruct typeConstruct ->
            Gen.ElmSyntaxTypeInfer.make_.typeConstruct
                (Elm.record
                    [ ( "moduleOrigin", typeConstruct.moduleOrigin |> moduleNameToExpression )
                    , ( "name", Elm.string typeConstruct.name )
                    , ( "arguments", Elm.list (typeConstruct.arguments |> List.map typeToExpression) )
                    ]
                )

        ElmSyntaxTypeInfer.TypeTuple typeTuple ->
            Gen.ElmSyntaxTypeInfer.make_.typeTuple
                (Elm.record
                    [ ( "part0", typeTuple.part0 |> typeToExpression )
                    , ( "part1", typeTuple.part1 |> typeToExpression )
                    ]
                )

        ElmSyntaxTypeInfer.TypeTriple typeTriple ->
            Gen.ElmSyntaxTypeInfer.make_.typeTriple
                (Elm.record
                    [ ( "part0", typeTriple.part0 |> typeToExpression )
                    , ( "part1", typeTriple.part1 |> typeToExpression )
                    , ( "part2", typeTriple.part2 |> typeToExpression )
                    ]
                )

        ElmSyntaxTypeInfer.TypeRecord fields ->
            Gen.ElmSyntaxTypeInfer.make_.typeRecord
                (Gen.FastDict.fromList
                    (fields
                        |> FastDict.toList
                        |> List.map
                            (\( fieldName, fieldValue ) ->
                                Elm.tuple
                                    (Elm.string fieldName)
                                    (fieldValue |> typeToExpression)
                            )
                    )
                )

        ElmSyntaxTypeInfer.TypeRecordExtension typeRecordExtension ->
            Gen.ElmSyntaxTypeInfer.make_.typeRecordExtension
                (Elm.record
                    [ ( "recordVariable"
                      , Elm.record
                          [ ( "name", Elm.string typeRecordExtension.recordVariable.name )
                          , ( "useRange"
                            , Elm.value
                              { importFrom = [ "Elm", "Syntax", "Range" ]
                              , name = "empty"
                              , annotation = Nothing
                              }
                            )
                          ]
                      )
                    , ( "fields"
                      , Gen.FastDict.fromList
                            (typeRecordExtension.fields
                                |> FastDict.toList
                                |> List.map
                                    (\( fieldName, fieldValue ) ->
                                        Elm.tuple
                                            (Elm.string fieldName)
                                            (fieldValue |> typeToExpression)
                                    )
                            )
                      )
                    ]
                )

        ElmSyntaxTypeInfer.TypeFunction typeFunction ->
            Gen.ElmSyntaxTypeInfer.make_.typeFunction
                (Elm.record
                    [ ( "input", typeFunction.input |> typeToExpression )
                    , ( "output", typeFunction.output |> typeToExpression )
                    ]
                )


moduleNameToExpression : Elm.Syntax.ModuleName.ModuleName -> Elm.Expression
moduleNameToExpression moduleName =
    Elm.list (moduleName |> List.map Elm.string)


interfaceToType : Elm.Type.Type -> Result String ElmSyntaxTypeInfer.Type
interfaceToType typeInterface =
    case typeInterface of
        Elm.Type.Var name ->
            Ok (ElmSyntaxTypeInfer.TypeVariable { useRange = Elm.Syntax.Range.empty, name = name })

        Elm.Type.Lambda functionInput functionOutput ->
            Result.map2
                (\input output ->
                    ElmSyntaxTypeInfer.TypeNotVariable
                        (ElmSyntaxTypeInfer.TypeFunction { input = input, output = output })
                )
                (functionInput |> interfaceToType)
                (functionOutput |> interfaceToType)

        Elm.Type.Tuple parts ->
            case parts of
                [] ->
                    Ok (ElmSyntaxTypeInfer.TypeNotVariable ElmSyntaxTypeInfer.TypeUnit)

                [ inParens ] ->
                    inParens |> interfaceToType

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeTuple { part0 = part0, part1 = part1 })
                        )
                        (tuplePart0 |> interfaceToType)
                        (tuplePart1 |> interfaceToType)

                [ triplePart0, triplePart1, triplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeTriple { part0 = part0, part1 = part1, part2 = part2 })
                        )
                        (triplePart0 |> interfaceToType)
                        (triplePart1 |> interfaceToType)
                        (triplePart2 |> interfaceToType)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Type.Type reference argumentInterfaces ->
            case reference |> String.split "." |> List.reverse of
                referenceName :: referenceModulePartLast :: referenceModulePartBeforeLastDown ->
                    Result.map
                        (\arguments ->
                            ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin =
                                        (referenceModulePartLast :: referenceModulePartBeforeLastDown)
                                            |> List.reverse
                                    , name = referenceName
                                    , arguments = arguments
                                    }
                                )
                        )
                        (argumentInterfaces |> listMapAndCombineOk interfaceToType)

                [ _ ] ->
                    Err "invalid reference"

                [] ->
                    Err "invalid reference"

        Elm.Type.Record fieldInterfaces Nothing ->
            Result.map
                (\fields -> ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeRecord fields))
                (fieldInterfaces
                    |> listMapAndCombineOk
                        (\( name, valueInterface ) ->
                            valueInterface
                                |> interfaceToType
                                |> Result.map (\value -> ( name, value ))
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Type.Record fieldInterfaces (Just extendedRecordVariable) ->
            Result.map
                (\fields ->
                    ElmSyntaxTypeInfer.TypeNotVariable
                        (ElmSyntaxTypeInfer.TypeRecordExtension { fields = fields, recordVariable = { useRange = Elm.Syntax.Range.empty, name = extendedRecordVariable } })
                )
                (fieldInterfaces
                    |> listMapAndCombineOk
                        (\( name, valueInterface ) ->
                            valueInterface
                                |> interfaceToType
                                |> Result.map (\value -> ( name, value ))
                        )
                    |> Result.map FastDict.fromList
                )


listMapAndCombineOk : (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOk elementToResult list =
    listMapAndCombineOkFrom [] elementToResult list


listMapAndCombineOkFrom : List ok -> (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOkFrom soFar elementToResult list =
    case list of
        [] ->
            Ok (soFar |> List.reverse)

        head :: tail ->
            case head |> elementToResult of
                Err headErr ->
                    Err headErr

                Ok headOk ->
                    listMapAndCombineOkFrom (headOk :: soFar)
                        elementToResult
                        tail
