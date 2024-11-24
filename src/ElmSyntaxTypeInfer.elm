module ElmSyntaxTypeInfer exposing
    ( ModuleTypes, elmCoreTypes, moduleDeclarationsToTypes, moduleInterfaceToTypes
    , importsToModuleOriginLookup, ModuleOriginLookup
    , expressionDeclaration, expressionDeclarations
    , TypedNode, Expression(..), LetDeclaration(..), Base10Or16(..), Pattern(..)
    , Type(..), TypeNotVariable(..), TypeVariableFromContext
    )

{-| Add type information to the nodes
of an [elm-syntax](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree.


## context

@docs ModuleTypes, elmCoreTypes, moduleDeclarationsToTypes, moduleInterfaceToTypes
@docs importsToModuleOriginLookup, ModuleOriginLookup


## syntax

@docs expressionDeclaration, expressionDeclarations
@docs TypedNode, Expression, LetDeclaration, Base10Or16, Pattern
@docs Type, TypeNotVariable, TypeVariableFromContext

-}

import Elm.Docs
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Import
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Elm.Type
import FastDict
import FastSet


{-| Container for known types of members of a module.
Create with [`moduleDeclarationsToTypes`](#moduleDeclarationsToTypes)
and [`moduleInterfaceToTypes`](#moduleInterfaceToTypes)
-}
type alias ModuleTypes =
    { signatures :
        -- value, function, port
        FastDict.Dict String (Type String)
    , typeAliases :
        FastDict.Dict
            String
            { parameters : List String
            , type_ : Type String
            }
    , choiceTypes :
        FastDict.Dict
            String
            { parameters : List String
            , variants :
                FastDict.Dict String (List (Type String))
            }
    }


{-| [`ModuleTypes`](#ModuleTypes) exposed in `elm/core`.

Please _always_ start with [`elmCoreTypes`](#elmCoreTypes)
and add further module info with [`FastDict.union`]()
using [`moduleDeclarationsToTypes`](#moduleDeclarationsToTypes)
and [`moduleInterfaceToTypes`](#moduleInterfaceToTypes)

And if for some reason you already know used dependencies at compile time,
you can re-use the [code generator used for these elm/core types](https://github.com/lue-bird/elm-syntax-type-infer/codegen)

-}
elmCoreTypes :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        ModuleTypes
elmCoreTypes =
    elmCoreTypesGeneratedFromDocsJson


{-| When syntax _introduces_ type variables from another "context" (another (let) declaration, another branch, another element, ...)
we need to differentiate these from variables in the current "context"

For example,

    ( identity, List.map identity )
    -- ( a -> a, List a -> List a )

would be an incorrect inference because the `a` in `identity` and `List.map` are not related
and can be different types.
So in practice these are

    ( identity, List.map identity )
    -- ( ( [ "first" ], "a" ) -> ( [ "first" ], "a" )
    -- , List ( [ "second", "0" ], "a" ) -> List ( [ "second", "0" ], "a" )
    -- )

`"first"` and `"second"` referring to the tuple part location
and `"0"` referring to the applied argument index.

We could work with some kind of name disambiguation system
but preserving names and context is usually nicer
for the final inferred variable names.

Why would you care?
[Types](#Type) inferred from [`expressionDeclaration`](#expressionDeclaration)
or [`expressionDeclarations`](#expressionDeclarations)
still contain these [`TypeVariableFromContext`](#TypeVariableFromContext)s
instead of just strings to preserve all that juicy information.
So you need to convert these yourself, like with something like

    typeVaiableFromContextToString : ElmSyntaxTypeInfer.TypeVariableFromContext -> String
    typeVaiableFromContextToString ( context, name ) =
        -- make sure the constraint like number is preserved
        name ++ (context |> List.map (\part -> "_" ++ part) |> String.concat)

Performance note: `ContextVariable` is a tuple to allow for internal use as a dict key.

-}
type alias TypeVariableFromContext =
    ( -- path inner to outer
      List String
    , String
    )


type TypeVariableConstraint
    = TypeVariableConstraintNumber
    | TypeVariableConstraintAppendable
    | TypeVariableConstraintComparable
    | TypeVariableConstraintCompappend


typeContextVariableName : TypeVariableFromContext -> String
typeContextVariableName ( _, name ) =
    name


typeVariableConstraint : String -> Maybe TypeVariableConstraint
typeVariableConstraint variableName =
    if variableName |> String.startsWith "number" then
        Nothing

    else if variableName |> String.startsWith "appendable" then
        Just TypeVariableConstraintAppendable

    else if variableName |> String.startsWith "comparable" then
        Just TypeVariableConstraintComparable

    else if variableName |> String.startsWith "compappend" then
        Just TypeVariableConstraintCompappend

    else
        Just TypeVariableConstraintNumber


maybeTypeVariableConstraintMerge : Maybe TypeVariableConstraint -> Maybe TypeVariableConstraint -> Result String (Maybe TypeVariableConstraint)
maybeTypeVariableConstraintMerge a b =
    case a of
        Nothing ->
            Ok b

        Just aConstraint ->
            case b of
                Nothing ->
                    Ok (Just aConstraint)

                Just bConstraint ->
                    typeVariableConstraintMerge aConstraint bConstraint
                        |> Result.map Just


typeVariableConstraintMerge : TypeVariableConstraint -> TypeVariableConstraint -> Result String TypeVariableConstraint
typeVariableConstraintMerge a b =
    case a of
        TypeVariableConstraintNumber ->
            case b of
                TypeVariableConstraintNumber ->
                    Ok TypeVariableConstraintNumber

                TypeVariableConstraintAppendable ->
                    Err "number and appendable variables cannot be unified"

                TypeVariableConstraintComparable ->
                    Ok TypeVariableConstraintNumber

                TypeVariableConstraintCompappend ->
                    Err "number and compappend variables cannot be unified"

        TypeVariableConstraintAppendable ->
            case b of
                TypeVariableConstraintNumber ->
                    Err "number and appendable variables cannot be unified"

                TypeVariableConstraintAppendable ->
                    Ok TypeVariableConstraintAppendable

                TypeVariableConstraintComparable ->
                    Ok TypeVariableConstraintCompappend

                TypeVariableConstraintCompappend ->
                    Ok TypeVariableConstraintCompappend

        TypeVariableConstraintComparable ->
            case b of
                TypeVariableConstraintNumber ->
                    Ok TypeVariableConstraintNumber

                TypeVariableConstraintAppendable ->
                    Ok TypeVariableConstraintCompappend

                TypeVariableConstraintComparable ->
                    Ok TypeVariableConstraintComparable

                TypeVariableConstraintCompappend ->
                    Ok TypeVariableConstraintCompappend

        TypeVariableConstraintCompappend ->
            case b of
                TypeVariableConstraintNumber ->
                    Err "number and compappend variables cannot be unified"

                TypeVariableConstraintAppendable ->
                    Ok TypeVariableConstraintCompappend

                TypeVariableConstraintComparable ->
                    Ok TypeVariableConstraintCompappend

                TypeVariableConstraintCompappend ->
                    Ok TypeVariableConstraintCompappend


{-| Type information attached to expressions and patterns,
see [`TypedNode`](#TypedNode).

This is different from [`Elm.Syntax.TypeAnnotation.TypeAnnotation`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation#TypeAnnotation)
in that it doesn't contain
information unrelated to type inference like ranges, qualification levels or parens.

-}
type Type variable
    = TypeVariable variable
    | TypeNotVariable (TypeNotVariable variable)


{-| [`Type`](#Type) except the variable case
-}
type TypeNotVariable variable
    = TypeUnit
    | TypeConstruct
        { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
        , name : String
        , arguments : List (Type variable)
        }
    | TypeTuple
        { part0 : Type variable
        , part1 : Type variable
        }
    | TypeTriple
        { part0 : Type variable
        , part1 : Type variable
        , part2 : Type variable
        }
    | TypeRecord (FastDict.Dict String (Type variable))
    | TypeRecordExtension
        { recordVariable : variable
        , fields : FastDict.Dict String (Type variable)
        }
    | TypeFunction
        { input : Type variable
        , output : Type variable
        }


typeVariablesMap :
    (variable -> variableMapped)
    -> Type variable
    -> Type variableMapped
typeVariablesMap variableMap type_ =
    -- IGNORE TCO
    case type_ of
        TypeVariable variable ->
            TypeVariable (variable |> variableMap)

        TypeNotVariable typeNotVariable ->
            TypeNotVariable
                (typeNotVariable
                    |> typeNotVariableVariablesMap variableMap
                )


typeNotVariableVariablesMap :
    (variable -> variableMapped)
    -> TypeNotVariable variable
    -> TypeNotVariable variableMapped
typeNotVariableVariablesMap variableMap typeNotVariable =
    case typeNotVariable of
        TypeUnit ->
            TypeUnit

        TypeConstruct typeConstruct ->
            TypeConstruct
                { moduleOrigin = typeConstruct.moduleOrigin
                , name = typeConstruct.name
                , arguments =
                    typeConstruct.arguments
                        |> List.map (\arg -> arg |> typeVariablesMap variableMap)
                }

        TypeTuple typeTuple ->
            TypeTuple
                { part0 = typeTuple.part0 |> typeVariablesMap variableMap
                , part1 = typeTuple.part1 |> typeVariablesMap variableMap
                }

        TypeTriple typeTriple ->
            TypeTriple
                { part0 = typeTriple.part0 |> typeVariablesMap variableMap
                , part1 = typeTriple.part1 |> typeVariablesMap variableMap
                , part2 = typeTriple.part2 |> typeVariablesMap variableMap
                }

        TypeRecord typeRecordFields ->
            TypeRecord
                (typeRecordFields
                    |> FastDict.map
                        (\_ fieldValue ->
                            fieldValue |> typeVariablesMap variableMap
                        )
                )

        TypeRecordExtension typeRecordExtension ->
            TypeRecordExtension
                { recordVariable =
                    typeRecordExtension.recordVariable
                        |> variableMap
                , fields =
                    typeRecordExtension.fields
                        |> FastDict.map
                            (\_ fieldValue ->
                                fieldValue |> typeVariablesMap variableMap
                            )
                }

        TypeFunction typeFunction ->
            TypeFunction
                { input = typeFunction.input |> typeVariablesMap variableMap
                , output = typeFunction.output |> typeVariablesMap variableMap
                }


{-| How do references used in a module map to their origin module?

Contains variants, type alias names, choice type names, port names, expression declaration names
and whether `(|.)` and or `(|=)` are imported from `Parser.Advanced`.

Also contains locally declared names when available.

-}
type alias ModuleOriginLookup =
    { references :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , keepOperatorIsExposedFromParserAdvanced : Bool
    , ignoreOperatorIsExposedFromParserAdvanced : Bool
    }


{-| Calculate valid mappings of qualifications + name
to origin module based on a module's imports.

Requires all exposed [`ModuleTypes`](#ModuleTypes)
so we can resolve `exposing (..)` and `ChoiceType(..)`.

-}
importsToModuleOriginLookup :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        ModuleTypes
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> ModuleOriginLookup
importsToModuleOriginLookup modulesTypes imports =
    let
        importsNormal :
            List
                { moduleName : Elm.Syntax.ModuleName.ModuleName
                , alias : Maybe String
                , exposes : List String
                }
        importsNormal =
            implicitImports
                ++ (imports
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ syntaxImport) ->
                                let
                                    importModuleName : Elm.Syntax.ModuleName.ModuleName
                                    importModuleName =
                                        syntaxImport.moduleName |> Elm.Syntax.Node.value
                                in
                                { moduleName = importModuleName
                                , alias =
                                    syntaxImport.moduleAlias
                                        |> Maybe.map
                                            (\(Elm.Syntax.Node.Node _ syntaxAlias) ->
                                                syntaxAlias |> String.join "."
                                            )
                                , exposes =
                                    case syntaxImport.exposingList of
                                        Nothing ->
                                            []

                                        Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                            case modulesTypes |> FastDict.get importModuleName of
                                                Nothing ->
                                                    []

                                                Just moduleTypes ->
                                                    case syntaxExposing of
                                                        Elm.Syntax.Exposing.All _ ->
                                                            (moduleTypes.signatures |> FastDict.keys)
                                                                ++ (moduleTypes.typeAliases |> FastDict.keys)
                                                                ++ (moduleTypes.choiceTypes
                                                                        |> FastDict.foldl
                                                                            (\choiceTypeName choiceTypeInfo soFar ->
                                                                                choiceTypeName
                                                                                    :: (choiceTypeInfo.variants |> FastDict.keys)
                                                                                    ++ soFar
                                                                            )
                                                                            []
                                                                   )

                                                        Elm.Syntax.Exposing.Explicit exposes ->
                                                            exposes
                                                                |> List.concatMap
                                                                    (\(Elm.Syntax.Node.Node _ expose) ->
                                                                        case expose of
                                                                            Elm.Syntax.Exposing.InfixExpose operator ->
                                                                                [ operator ]

                                                                            Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                [ name ]

                                                                            Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                                                                [ name ]

                                                                            Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                                                                                case choiceTypeExpose.open of
                                                                                    Nothing ->
                                                                                        [ choiceTypeExpose.name ]

                                                                                    Just _ ->
                                                                                        case moduleTypes.choiceTypes |> FastDict.get choiceTypeExpose.name of
                                                                                            Nothing ->
                                                                                                []

                                                                                            Just choiceTypeDeclared ->
                                                                                                choiceTypeDeclared.variants |> FastDict.keys
                                                                    )
                                }
                            )
                   )
                |> importsCombine

        operatorIsExposedFromParserAdvanced : String -> Bool
        operatorIsExposedFromParserAdvanced operator =
            importsNormal
                |> List.any
                    (\syntaxImport ->
                        (case syntaxImport.moduleName of
                            [ "Parser", "Advanced" ] ->
                                True

                            _ ->
                                False
                        )
                            && (syntaxImport.exposes
                                    |> List.any
                                        (\syntaxExpose ->
                                            (syntaxExpose == operator)
                                                || (syntaxExpose == ("(" ++ operator ++ ")"))
                                        )
                               )
                    )
    in
    { references =
        importsNormal
            |> List.foldl
                (\syntaxImport soFar ->
                    case modulesTypes |> FastDict.get syntaxImport.moduleName of
                        Nothing ->
                            soFar

                        Just moduleTypes ->
                            let
                                exposedFromImportedModuleItself : List String
                                exposedFromImportedModuleItself =
                                    (moduleTypes.signatures |> FastDict.keys)
                                        ++ (moduleTypes.typeAliases |> FastDict.keys)
                                        ++ (moduleTypes.choiceTypes
                                                |> FastDict.foldl
                                                    (\choiceTypeName choiceType variantNamesSoFar ->
                                                        choiceTypeName
                                                            :: (choiceType.variants |> FastDict.keys)
                                                            ++ variantNamesSoFar
                                                    )
                                                    []
                                           )
                            in
                            FastDict.union
                                (FastDict.union
                                    (syntaxImport.exposes
                                        |> listMapToFastDict
                                            (\expose ->
                                                { key = ( [], expose )
                                                , value = syntaxImport.moduleName
                                                }
                                            )
                                    )
                                    (case syntaxImport.alias of
                                        Nothing ->
                                            exposedFromImportedModuleItself
                                                |> listMapToFastDict
                                                    (\exposeFromImportedModule ->
                                                        { key = ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        , value = syntaxImport.moduleName
                                                        }
                                                    )

                                        Just importAlias ->
                                            exposedFromImportedModuleItself
                                                |> listMapToFastDict
                                                    (\exposeFromImportedModule ->
                                                        { key = ( [ importAlias ], exposeFromImportedModule )
                                                        , value = syntaxImport.moduleName
                                                        }
                                                    )
                                    )
                                )
                                soFar
                )
                FastDict.empty
    , keepOperatorIsExposedFromParserAdvanced =
        operatorIsExposedFromParserAdvanced "|."
    , ignoreOperatorIsExposedFromParserAdvanced =
        operatorIsExposedFromParserAdvanced "|="
    }


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : List String
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
      , exposes =
            [ "Int"
            , "Float"
            , "(+)"
            , "(-)"
            , "(*)"
            , "(/)"
            , "(//)"
            , "(^)"
            , "toFloat"
            , "round"
            , "floor"
            , "ceiling"
            , "truncate"
            , "(==)"
            , "(/=)"
            , "(<)"
            , "(>)"
            , "(<=)"
            , "(>=)"
            , "max"
            , "min"
            , "compare"
            , "Order"
            , "LT"
            , "EQ"
            , "GT"
            , "Bool"
            , "True"
            , "False"
            , "not"
            , "(&&)"
            , "(||)"
            , "xor"
            , "(++)"
            , "modBy"
            , "remainderBy"
            , "negate"
            , "abs"
            , "clamp"
            , "sqrt"
            , "logBase"
            , "e"
            , "pi"
            , "cos"
            , "sin"
            , "tan"
            , "acos"
            , "asin"
            , "atan"
            , "atan2"
            , "degrees"
            , "radians"
            , "turns"
            , "toPolar"
            , "fromPolar"
            , "isNaN"
            , "isInfinite"
            , "identity"
            , "always"
            , "(<|)"
            , "(|>)"
            , "(<<)"
            , "(>>)"
            , "Never"
            , "never"
            ]
      }
    , { moduleName = [ "List" ], alias = Nothing, exposes = [ "List", "(::)" ] }
    , { moduleName = [ "Maybe" ], alias = Nothing, exposes = [ "Maybe", "Just", "Nothing" ] }
    , { moduleName = [ "Result" ], alias = Nothing, exposes = [ "Result", "Ok", "Err" ] }
    , { moduleName = [ "String" ], alias = Nothing, exposes = [ "String" ] }
    , { moduleName = [ "Char" ], alias = Nothing, exposes = [ "Char" ] }
    , { moduleName = [ "Tuple" ], alias = Nothing, exposes = [] }
    , { moduleName = [ "Debug" ], alias = Nothing, exposes = [] }
    , { moduleName = [ "Platform" ], alias = Nothing, exposes = [ "Program" ] }
    , { moduleName = [ "Platform", "Cmd" ], alias = Just "Cmd", exposes = [ "Cmd" ] }
    , { moduleName = [ "Platform", "Sub" ], alias = Just "Sub", exposes = [ "Sub" ] }
    ]


listMapToFastDict :
    (a -> { key : comparableKey, value : value })
    -> List a
    -> FastDict.Dict comparableKey value
listMapToFastDict elementToKeyValue list =
    list
        |> List.foldl
            (\element soFar ->
                let
                    keyValue : { key : comparableKey, value : value }
                    keyValue =
                        element |> elementToKeyValue
                in
                soFar |> FastDict.insert keyValue.key keyValue.value
            )
            FastDict.empty


importsCombine :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : List String
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposes : List String
            }
importsCombine syntaxImports =
    importsCombineFrom [] syntaxImports


importsCombineFrom :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : List String
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposes : List String
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposes : List String
            }
importsCombineFrom soFar syntaxImports =
    case syntaxImports of
        [] ->
            soFar

        [ onlyImport ] ->
            onlyImport :: soFar

        import0 :: import1 :: import2Up ->
            if import0.moduleName == import1.moduleName then
                importsCombineFrom soFar
                    (importsMerge import0 import1
                        :: import2Up
                    )

            else
                importsCombineFrom
                    (import0 :: soFar)
                    (import1 :: import2Up)


importsMerge :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposes : List String
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : List String
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : List String
        }
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , alias =
        case earlier.alias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.alias
    , exposes =
        exposingCombine earlier.exposes later.exposes
    }


exposingCombine : List String -> List String -> List String
exposingCombine a b =
    a ++ b |> exposeListToNormal


exposeListToNormal :
    List String
    -> List String
exposeListToNormal syntaxExposeList =
    syntaxExposeList
        |> List.sort
        |> exposesCombine


exposesCombine : List String -> List String
exposesCombine syntaxExposes =
    exposesCombineFrom [] syntaxExposes


exposesCombineFrom : List String -> List String -> List String
exposesCombineFrom soFar syntaxExposes =
    case syntaxExposes of
        [] ->
            soFar

        [ onlyExpose ] ->
            onlyExpose :: soFar

        expose0 :: expose1 :: expose2Up ->
            case Basics.compare expose0 expose1 of
                EQ ->
                    exposesCombineFrom soFar (expose0 :: expose2Up)

                LT ->
                    exposesCombineFrom (expose0 :: soFar) (expose1 :: expose2Up)

                GT ->
                    exposesCombineFrom (expose0 :: soFar) (expose1 :: expose2Up)


syntaxToType :
    ModuleOriginLookup
    -> Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String (Type String)
syntaxToType moduleOriginLookup syntaxType =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Ok (TypeNotVariable TypeUnit)

        Elm.Syntax.TypeAnnotation.GenericType variableName ->
            Ok (TypeVariable variableName)

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( qualification, unqualifiedName )) argumentNodes ->
            case moduleOriginLookup.references |> FastDict.get ( qualification, unqualifiedName ) of
                Nothing ->
                    Err
                        ("could not find imported/local declaration for "
                            ++ qualifiedToString { qualification = qualification, name = unqualifiedName }
                        )

                Just originModule ->
                    argumentNodes
                        |> listMapAndCombineOk
                            (\(Elm.Syntax.Node.Node _ argument) ->
                                argument |> syntaxToType moduleOriginLookup
                            )
                        |> Result.map
                            (\arguments ->
                                TypeNotVariable
                                    (TypeConstruct
                                        { moduleOrigin = originModule
                                        , name = unqualifiedName
                                        , arguments = arguments
                                        }
                                    )
                            )

        Elm.Syntax.TypeAnnotation.Tupled tupleParts ->
            case tupleParts of
                [] ->
                    Err "empty tuple"

                [ Elm.Syntax.Node.Node _ inParens ] ->
                    inParens |> syntaxToType moduleOriginLookup

                [ Elm.Syntax.Node.Node _ syntaxPart0, Elm.Syntax.Node.Node _ syntaxPart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            TypeNotVariable
                                (TypeTuple { part0 = part0, part1 = part1 })
                        )
                        (syntaxPart0 |> syntaxToType moduleOriginLookup)
                        (syntaxPart1 |> syntaxToType moduleOriginLookup)

                [ Elm.Syntax.Node.Node _ syntaxPart0, Elm.Syntax.Node.Node _ syntaxPart1, Elm.Syntax.Node.Node _ syntaxPart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            TypeNotVariable
                                (TypeTriple { part0 = part0, part1 = part1, part2 = part2 })
                        )
                        (syntaxPart0 |> syntaxToType moduleOriginLookup)
                        (syntaxPart1 |> syntaxToType moduleOriginLookup)
                        (syntaxPart2 |> syntaxToType moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.TypeAnnotation.Record recordFields ->
            Result.map
                (\fields -> TypeNotVariable (TypeRecord fields))
                (recordFields
                    |> listFoldlWhileOkFrom FastDict.empty
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, Elm.Syntax.Node.Node _ fieldValue )) soFar ->
                            fieldValue
                                |> syntaxToType moduleOriginLookup
                                |> Result.map
                                    (\fieldValueType ->
                                        soFar |> FastDict.insert fieldName fieldValueType
                                    )
                        )
                )

        Elm.Syntax.TypeAnnotation.GenericRecord (Elm.Syntax.Node.Node _ recordVariableName) (Elm.Syntax.Node.Node _ recordExtensionFields) ->
            Result.map
                (\fields ->
                    TypeNotVariable
                        (TypeRecordExtension
                            { recordVariable = recordVariableName
                            , fields = fields
                            }
                        )
                )
                (recordExtensionFields
                    |> listFoldlWhileOkFrom
                        FastDict.empty
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, Elm.Syntax.Node.Node _ fieldValue )) soFar ->
                            fieldValue
                                |> syntaxToType moduleOriginLookup
                                |> Result.map
                                    (\fieldValueType ->
                                        soFar |> FastDict.insert fieldName fieldValueType
                                    )
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (Elm.Syntax.Node.Node _ syntaxInput) (Elm.Syntax.Node.Node _ syntaxOutput) ->
            Result.map2
                (\input output ->
                    TypeNotVariable
                        (TypeFunction { input = input, output = output })
                )
                (syntaxInput |> syntaxToType moduleOriginLookup)
                (syntaxOutput |> syntaxToType moduleOriginLookup)


qualifiedToString :
    { qualification : Elm.Syntax.ModuleName.ModuleName, name : String }
    -> String
qualifiedToString reference =
    case reference.qualification of
        [] ->
            reference.name

        qualificationUntilDot :: qualificationAfterDot ->
            ((qualificationUntilDot :: qualificationAfterDot)
                |> String.join "."
            )
                ++ "."
                ++ reference.name


typeSubstituteVariableBy :
    ModuleLevelDeclarationTypesInAvailableInModule
    ->
        { variable : TypeVariableFromContext
        , type_ : Type TypeVariableFromContext
        }
    -> Type TypeVariableFromContext
    ->
        Result
            String
            { type_ : Type TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeSubstituteVariableBy declarationTypes replacement type_ =
    case replacement.type_ of
        TypeVariable argumentVariable ->
            Ok
                { type_ =
                    type_
                        |> typeVariablesMap
                            (\variable ->
                                if variable == replacement.variable then
                                    argumentVariable

                                else
                                    variable
                            )
                , substitutions = variableSubstitutionsNone
                }

        TypeNotVariable argumentNotVariable ->
            type_
                |> typeSubstituteVariableByNotVariable declarationTypes
                    { variable = replacement.variable
                    , type_ = argumentNotVariable
                    }


typeSubstituteVariableByNotVariable :
    ModuleLevelDeclarationTypesInAvailableInModule
    ->
        { variable : TypeVariableFromContext
        , type_ : TypeNotVariable TypeVariableFromContext
        }
    -> Type TypeVariableFromContext
    ->
        Result
            String
            { type_ : Type TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeSubstituteVariableByNotVariable declarationTypes replacement type_ =
    -- IGNORE TCO
    case type_ of
        TypeVariable typeVariable ->
            if typeVariable == replacement.variable then
                case replacement.variable |> typeContextVariableName |> typeVariableConstraint of
                    Nothing ->
                        Ok
                            { type_ = TypeNotVariable replacement.type_
                            , substitutions = variableSubstitutionsNone
                            }

                    Just constraint ->
                        case constraint of
                            TypeVariableConstraintNumber ->
                                if replacement.type_ |> typeNotVariableIsNumber then
                                    Ok
                                        { type_ = TypeNotVariable replacement.type_
                                        , substitutions = variableSubstitutionsNone
                                        }

                                else
                                    Err "cannot unify number type variable with types other than Int/Float"

                            TypeVariableConstraintAppendable ->
                                if replacement.type_ |> typeNotVariableIsAppendable then
                                    Ok
                                        { type_ = TypeNotVariable replacement.type_
                                        , substitutions = variableSubstitutionsNone
                                        }

                                else
                                    Err "cannot unify appendable type variable with types other than String/List _"

                            TypeVariableConstraintComparable ->
                                if replacement.type_ |> typeNotVariableIsComparable (\var -> var |> typeContextVariableName |> typeVariableConstraint) then
                                    Ok
                                        { type_ = TypeNotVariable replacement.type_
                                        , substitutions = variableSubstitutionsNone
                                        }

                                else
                                    Err "cannot unify comparable type variable with types other than Int/Float/String/Time.Posix/List of comparable/tuple of comparables/triple of comparable"

                            TypeVariableConstraintCompappend ->
                                if replacement.type_ |> typeNotVariableIsCompappend (\var -> var |> typeContextVariableName |> typeVariableConstraint) then
                                    Ok
                                        { type_ = TypeNotVariable replacement.type_
                                        , substitutions = variableSubstitutionsNone
                                        }

                                else
                                    Err "cannot unify compappend type variable with types other than String/List of comparable"

            else
                Ok
                    { type_ = TypeVariable typeVariable
                    , substitutions = variableSubstitutionsNone
                    }

        TypeNotVariable typeNotVariable ->
            typeNotVariable
                |> typeNotVariableSubstituteVariable declarationTypes
                    replacement
                |> Result.map
                    (\typeAndSubstitutions ->
                        { type_ = TypeNotVariable typeAndSubstitutions.type_
                        , substitutions = typeAndSubstitutions.substitutions
                        }
                    )


typeNotVariableSubstituteVariable :
    ModuleLevelDeclarationTypesInAvailableInModule
    ->
        { variable : TypeVariableFromContext
        , type_ : TypeNotVariable TypeVariableFromContext
        }
    -> TypeNotVariable TypeVariableFromContext
    ->
        Result
            String
            { type_ : TypeNotVariable TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeNotVariableSubstituteVariable declarationTypes replacement typeNotVariable =
    case typeNotVariable of
        TypeUnit ->
            Ok
                { type_ = TypeUnit
                , substitutions = variableSubstitutionsNone
                }

        TypeConstruct typeChoiceConstruct ->
            Result.map
                (\argumentsSubstituted ->
                    { type_ =
                        TypeConstruct
                            { moduleOrigin = typeChoiceConstruct.moduleOrigin
                            , name = typeChoiceConstruct.name
                            , arguments =
                                argumentsSubstituted.argumentsReverse
                                    |> List.reverse
                            }
                    , substitutions = argumentsSubstituted.substitutions
                    }
                )
                (typeChoiceConstruct.arguments
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , argumentsReverse = []
                        }
                        (\argument soFar ->
                            argument
                                |> typeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\argumentSubstituted ->
                                        variableSubstitutionsMerge declarationTypes
                                            argumentSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\substitutionsWithArgument ->
                                                    { argumentsReverse =
                                                        argumentSubstituted.type_
                                                            :: soFar.argumentsReverse
                                                    , substitutions = substitutionsWithArgument
                                                    }
                                                )
                                    )
                        )
                )

        TypeTuple typeTuple ->
            Result.map2
                (\part0Substituted part1Substituted ->
                    variableSubstitutionsMerge declarationTypes
                        part0Substituted.substitutions
                        part1Substituted.substitutions
                        |> Result.map
                            (\substitutionsPart01 ->
                                { type_ =
                                    TypeTuple
                                        { part0 = part0Substituted.type_
                                        , part1 = part1Substituted.type_
                                        }
                                , substitutions = substitutionsPart01
                                }
                            )
                )
                (typeTuple.part0
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (typeTuple.part1
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        TypeTriple typeTriple ->
            Result.map3
                (\part0Substituted part1Substituted part2Substituted ->
                    variableSubstitutionsMerge3 declarationTypes
                        part0Substituted.substitutions
                        part1Substituted.substitutions
                        part2Substituted.substitutions
                        |> Result.map
                            (\substitutionsPart01 ->
                                { type_ =
                                    TypeTriple
                                        { part0 = part0Substituted.type_
                                        , part1 = part1Substituted.type_
                                        , part2 = part2Substituted.type_
                                        }
                                , substitutions = substitutionsPart01
                                }
                            )
                )
                (typeTriple.part0
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (typeTriple.part1
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (typeTriple.part2
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        TypeRecord typeRecordFields ->
            typeRecordFields
                |> fastDictFoldlWhileOkFrom
                    { substitutions = variableSubstitutionsNone
                    , types = FastDict.empty
                    }
                    (\fieldName fieldValue soFar ->
                        fieldValue
                            |> typeSubstituteVariableByNotVariable declarationTypes
                                replacement
                            |> Result.andThen
                                (\valueSubstituted ->
                                    variableSubstitutionsMerge declarationTypes
                                        valueSubstituted.substitutions
                                        soFar.substitutions
                                        |> Result.map
                                            (\substitutionsWithValue ->
                                                { substitutions = substitutionsWithValue
                                                , types =
                                                    soFar.types
                                                        |> FastDict.insert fieldName valueSubstituted.type_
                                                }
                                            )
                                )
                    )
                |> Result.map
                    (\fieldsSubstituted ->
                        { substitutions = fieldsSubstituted.substitutions
                        , type_ = TypeRecord fieldsSubstituted.types
                        }
                    )

        TypeRecordExtension typeRecordExtension ->
            typeRecordExtension.fields
                |> fastDictFoldlWhileOkFrom
                    { substitutions = variableSubstitutionsNone
                    , types = FastDict.empty
                    }
                    (\fieldName fieldValue soFar ->
                        fieldValue
                            |> typeSubstituteVariableByNotVariable declarationTypes
                                replacement
                            |> Result.andThen
                                (\valueSubstituted ->
                                    variableSubstitutionsMerge declarationTypes
                                        soFar.substitutions
                                        valueSubstituted.substitutions
                                        |> Result.map
                                            (\substitutionsWithValue ->
                                                { substitutions = substitutionsWithValue
                                                , types =
                                                    soFar.types
                                                        |> FastDict.insert fieldName valueSubstituted.type_
                                                }
                                            )
                                )
                    )
                |> Result.andThen
                    (\fieldsSubstituted ->
                        if typeRecordExtension.recordVariable /= replacement.variable then
                            Ok
                                { substitutions = fieldsSubstituted.substitutions
                                , type_ =
                                    TypeRecordExtension
                                        { recordVariable = typeRecordExtension.recordVariable
                                        , fields = fieldsSubstituted.types
                                        }
                                }

                        else
                            case replacement.type_ of
                                TypeRecord replacementFields ->
                                    typeRecordUnify declarationTypes
                                        replacementFields
                                        fieldsSubstituted.types
                                        |> Result.andThen
                                            (\recordSubstitutedRecordUnified ->
                                                variableSubstitutionsMerge declarationTypes
                                                    fieldsSubstituted.substitutions
                                                    recordSubstitutedRecordUnified.substitutions
                                                    |> Result.map
                                                        (\fullSubstitutions ->
                                                            { substitutions = fullSubstitutions
                                                            , type_ = recordSubstitutedRecordUnified.type_
                                                            }
                                                        )
                                            )

                                TypeRecordExtension replacementRecordExtension ->
                                    typeRecordExtensionRecordUnify declarationTypes
                                        { recordVariable = replacementRecordExtension.recordVariable
                                        , fields = replacementRecordExtension.fields
                                        }
                                        fieldsSubstituted.types
                                        |> Result.andThen
                                            (\recordSubstitutedRecordExtensionUnified ->
                                                variableSubstitutionsMerge declarationTypes
                                                    fieldsSubstituted.substitutions
                                                    recordSubstitutedRecordExtensionUnified.substitutions
                                                    |> Result.map
                                                        (\fullSubstitutions ->
                                                            { substitutions = fullSubstitutions
                                                            , type_ = recordSubstitutedRecordExtensionUnified.type_
                                                            }
                                                        )
                                            )

                                _ ->
                                    Err "cannot unify record extension type variable with types other than record/record extension"
                    )

        TypeFunction typeFunction ->
            Result.map2
                (\inputSubstituted outputSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        inputSubstituted.substitutions
                        outputSubstituted.substitutions
                        |> Result.map
                            (\substitutionsInputOutput ->
                                { type_ =
                                    TypeFunction
                                        { input = inputSubstituted.type_
                                        , output = outputSubstituted.type_
                                        }
                                , substitutions = substitutionsInputOutput
                                }
                            )
                )
                (typeFunction.input
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (typeFunction.output
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity


typeNotVariableIsNumber : TypeNotVariable variable_ -> Bool
typeNotVariableIsNumber type_ =
    (type_
        == TypeConstruct
            { moduleOrigin = [ "Basics" ]
            , name = "Int"
            , arguments = []
            }
    )
        || (type_
                == TypeConstruct
                    { moduleOrigin = [ "Basics" ]
                    , name = "Float"
                    , arguments = []
                    }
           )


typeNotVariableIsAppendable : TypeNotVariable variable_ -> Bool
typeNotVariableIsAppendable type_ =
    case type_ of
        TypeConstruct variableReplacementTypeChoiceConstruct ->
            case variableReplacementTypeChoiceConstruct.moduleOrigin of
                [ "Basics" ] ->
                    case variableReplacementTypeChoiceConstruct.name of
                        "String" ->
                            True

                        _ ->
                            False

                [ "List" ] ->
                    case variableReplacementTypeChoiceConstruct.name of
                        "List" ->
                            True

                        _ ->
                            False

                _ ->
                    False

        TypeUnit ->
            False

        TypeTuple _ ->
            False

        TypeTriple _ ->
            False

        TypeRecord _ ->
            False

        TypeRecordExtension _ ->
            False

        TypeFunction _ ->
            False


typeNotVariableIsComparable :
    (variable -> Maybe TypeVariableConstraint)
    -> TypeNotVariable variable
    -> Bool
typeNotVariableIsComparable typeVariableToConstraint typeNotVariable =
    -- IGNORE TCO
    case typeNotVariable of
        TypeConstruct variableReplacementTypeChoiceConstruct ->
            case variableReplacementTypeChoiceConstruct.moduleOrigin of
                [ "Basics" ] ->
                    case variableReplacementTypeChoiceConstruct.name of
                        "String" ->
                            True

                        "Int" ->
                            True

                        "Float" ->
                            True

                        _ ->
                            False

                [ "Time" ] ->
                    case variableReplacementTypeChoiceConstruct.name of
                        "Posix" ->
                            True

                        _ ->
                            False

                [ "List" ] ->
                    case variableReplacementTypeChoiceConstruct.name of
                        "List" ->
                            variableReplacementTypeChoiceConstruct.arguments
                                |> List.all
                                    (\argument ->
                                        argument |> typeIsComparable typeVariableToConstraint
                                    )

                        _ ->
                            False

                _ ->
                    False

        TypeTuple typeTuple ->
            (typeTuple.part0 |> typeIsComparable typeVariableToConstraint)
                && (typeTuple.part1 |> typeIsComparable typeVariableToConstraint)

        TypeTriple typeTriple ->
            (typeTriple.part0 |> typeIsComparable typeVariableToConstraint)
                && (typeTriple.part1 |> typeIsComparable typeVariableToConstraint)
                && (typeTriple.part2 |> typeIsComparable typeVariableToConstraint)

        TypeUnit ->
            False

        TypeRecord _ ->
            False

        TypeRecordExtension _ ->
            False

        TypeFunction _ ->
            False


typeIsComparable :
    (variable -> Maybe TypeVariableConstraint)
    -> Type variable
    -> Bool
typeIsComparable typeVariableToConstraint type_ =
    case type_ of
        TypeVariable typeVariable ->
            case typeVariable |> typeVariableToConstraint of
                Nothing ->
                    False

                Just TypeVariableConstraintAppendable ->
                    True

                Just TypeVariableConstraintCompappend ->
                    True

                Just TypeVariableConstraintComparable ->
                    True

                Just TypeVariableConstraintNumber ->
                    True

        TypeNotVariable typeNotVariable ->
            typeNotVariable |> typeNotVariableIsComparable typeVariableToConstraint


typeNotVariableIsCompappend :
    (variable -> Maybe TypeVariableConstraint)
    -> TypeNotVariable variable
    -> Bool
typeNotVariableIsCompappend typeVariableToConstraint type_ =
    -- IGNORE TCO
    case type_ of
        TypeConstruct variableReplacementTypeChoiceConstruct ->
            case variableReplacementTypeChoiceConstruct.moduleOrigin of
                [ "Basics" ] ->
                    case variableReplacementTypeChoiceConstruct.name of
                        "String" ->
                            True

                        _ ->
                            False

                [ "List" ] ->
                    case variableReplacementTypeChoiceConstruct.name of
                        "List" ->
                            variableReplacementTypeChoiceConstruct.arguments
                                |> List.all
                                    (\argument ->
                                        argument |> typeIsComparable typeVariableToConstraint
                                    )

                        _ ->
                            False

                _ ->
                    False

        TypeUnit ->
            False

        TypeTuple _ ->
            False

        TypeTriple _ ->
            False

        TypeRecord _ ->
            False

        TypeRecordExtension _ ->
            False

        TypeFunction _ ->
            False


{-| All you need to turn a generic type with variables
into a concrete type with all the info we've inferred already.

While variable types in context get passed down,
variable substitutions get passed all the way to the top and only get processed there.

TODO don't forget to apply variable to type substitutions for _all_ equivalent variables!
TODO don't forget to apply variable to type substitutions for other variable to type substitutions!

-}
type alias VariableSubstitutions =
    { equivalentVariables :
        List (FastSet.Set TypeVariableFromContext)
    , variableToType :
        FastDict.Dict
            TypeVariableFromContext
            (TypeNotVariable TypeVariableFromContext)
    }


variableSubstitutionsNone : VariableSubstitutions
variableSubstitutionsNone =
    { equivalentVariables = []
    , variableToType = FastDict.empty
    }


variableSubstitutionsMerge :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> Result String VariableSubstitutions
variableSubstitutionsMerge declarationTypes a b =
    -- IGNORE TCO
    FastDict.merge
        (\variable aType soFar ->
            soFar
                |> Result.map
                    (\soFarVariablesToTypeAndSubstitutions ->
                        { variableToType =
                            soFarVariablesToTypeAndSubstitutions.variableToType
                                |> FastDict.insert variable aType
                        , equivalentVariables =
                            soFarVariablesToTypeAndSubstitutions.equivalentVariables
                        }
                    )
        )
        (\variable aType bType soFar ->
            soFar
                |> Result.andThen
                    (\soFarVariablesToTypeAndSubstitutions ->
                        case typeNotVariableUnify declarationTypes aType bType of
                            Err error ->
                                Err error

                            Ok abTypesUnified ->
                                variableSubstitutionsMerge declarationTypes
                                    soFarVariablesToTypeAndSubstitutions
                                    abTypesUnified.substitutions
                                    |> Result.map
                                        (\substitutionsWithAB ->
                                            case abTypesUnified.type_ of
                                                TypeVariable abUnifiedVariable ->
                                                    { equivalentVariables =
                                                        substitutionsWithAB.equivalentVariables
                                                            |> equivalentVariablesInsert
                                                                (FastSet.singleton variable
                                                                    |> FastSet.insert abUnifiedVariable
                                                                )
                                                    , variableToType =
                                                        substitutionsWithAB.variableToType
                                                    }

                                                TypeNotVariable abUnifiedNotVariable ->
                                                    { equivalentVariables = substitutionsWithAB.equivalentVariables
                                                    , variableToType =
                                                        substitutionsWithAB.variableToType
                                                            |> FastDict.insert variable abUnifiedNotVariable
                                                    }
                                        )
                    )
        )
        (\variable bType soFar ->
            soFar
                |> Result.map
                    (\soFarVariablesToTypeAndSubstitutions ->
                        { variableToType =
                            soFarVariablesToTypeAndSubstitutions.variableToType
                                |> FastDict.insert variable bType
                        , equivalentVariables =
                            soFarVariablesToTypeAndSubstitutions.equivalentVariables
                        }
                    )
        )
        a.variableToType
        b.variableToType
        (Ok
            { variableToType = FastDict.empty
            , equivalentVariables =
                equivalentVariableSetMerge
                    a.equivalentVariables
                    b.equivalentVariables
            }
        )


variableSubstitutionsMerge3 :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> Result String VariableSubstitutions
variableSubstitutionsMerge3 declarationTypes a b c =
    variableSubstitutionsMerge
        declarationTypes
        a
        b
        |> Result.andThen
            (\abSubstitutions ->
                variableSubstitutionsMerge
                    declarationTypes
                    abSubstitutions
                    c
            )


variableSubstitutionsMerge4 :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> Result String VariableSubstitutions
variableSubstitutionsMerge4 declarationTypes a b c d =
    variableSubstitutionsMerge3
        declarationTypes
        a
        b
        c
        |> Result.andThen
            (\abcSubstitutions ->
                variableSubstitutionsMerge
                    declarationTypes
                    abcSubstitutions
                    d
            )


equivalentVariablesInsert :
    FastSet.Set comparable
    -> List (FastSet.Set comparable)
    -> List (FastSet.Set comparable)
equivalentVariablesInsert equivalentVariablesSet equivalentVariables =
    -- TODO optimize?
    equivalentVariableSetMerge
        equivalentVariables
        [ equivalentVariablesSet ]


equivalentVariableSetMerge :
    List (FastSet.Set comparable)
    -> List (FastSet.Set comparable)
    -> List (FastSet.Set comparable)
equivalentVariableSetMerge a b =
    let
        mergedIntoA :
            { sets : List (FastSet.Set comparable)
            , bRemaining : List (FastSet.Set comparable)
            }
        mergedIntoA =
            a
                |> List.foldl
                    (\aEquivalentVariableSet soFar ->
                        case
                            soFar.bRemaining
                                |> listMapAndFirstJustAndRemainingAnyOrder
                                    (\bEquivalentVariableSet ->
                                        if fastSetShareElements aEquivalentVariableSet bEquivalentVariableSet then
                                            Just bEquivalentVariableSet

                                        else
                                            Nothing
                                    )
                        of
                            Nothing ->
                                { sets = aEquivalentVariableSet :: soFar.sets
                                , bRemaining = soFar.bRemaining
                                }

                            Just bEquivalentVariableSetAndRemaining ->
                                { sets =
                                    FastSet.union aEquivalentVariableSet bEquivalentVariableSetAndRemaining.value
                                        :: soFar.sets
                                , bRemaining = bEquivalentVariableSetAndRemaining.remaining
                                }
                    )
                    { sets = [], bRemaining = b }
    in
    -- TODO remaining in b
    mergedIntoA.sets
        ++ mergedIntoA.bRemaining


fastSetShareElements : FastSet.Set comparable -> FastSet.Set comparable -> Bool
fastSetShareElements a b =
    FastSet.intersect a b /= FastSet.empty


listMapAndFirstJustAndRemainingAnyOrder :
    (a -> Maybe value)
    -> List a
    ->
        Maybe
            { value : value
            , remaining : List a
            }
listMapAndFirstJustAndRemainingAnyOrder elementToMaybe list =
    listMapAndFirstJustAndRemainingAndOrderWithBefore [] elementToMaybe list


listMapAndFirstJustAndRemainingAndOrderWithBefore :
    List a
    -> (a -> Maybe value)
    -> List a
    ->
        Maybe
            { value : value
            , remaining : List a
            }
listMapAndFirstJustAndRemainingAndOrderWithBefore elementsBeforeReverse elementToMaybe list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case head |> elementToMaybe of
                Just headValue ->
                    Just
                        { value = headValue
                        , remaining =
                            (elementsBeforeReverse |> List.reverse)
                                ++ tail
                        }

                Nothing ->
                    listMapAndFirstJustAndRemainingAndOrderWithBefore
                        (head :: elementsBeforeReverse)
                        elementToMaybe
                        tail


typeUnify :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> Type TypeVariableFromContext
    -> Type TypeVariableFromContext
    ->
        Result
            String
            { type_ : Type TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeUnify declarationTypes a b =
    case a of
        TypeNotVariable aTypeNotVariable ->
            case b of
                TypeVariable bVariableName ->
                    Ok
                        { type_ = TypeNotVariable aTypeNotVariable
                        , substitutions =
                            { variableToType =
                                FastDict.singleton bVariableName
                                    aTypeNotVariable
                            , equivalentVariables = []
                            }
                        }

                TypeNotVariable bTypeNotVariable ->
                    case typeNotVariableUnify declarationTypes aTypeNotVariable bTypeNotVariable of
                        Err error ->
                            Err error

                        Ok unifiedNotVariable ->
                            Ok
                                { type_ = unifiedNotVariable.type_
                                , substitutions = unifiedNotVariable.substitutions
                                }

        TypeVariable aVariable ->
            case b of
                TypeVariable bVariable ->
                    Ok
                        { type_ = TypeVariable aVariable
                        , substitutions =
                            { variableToType = FastDict.empty
                            , equivalentVariables =
                                [ FastSet.singleton aVariable
                                    |> FastSet.insert bVariable
                                ]
                            }
                        }

                TypeNotVariable bTypeNotVariable ->
                    Ok
                        { type_ = TypeVariable aVariable
                        , substitutions =
                            { variableToType =
                                FastDict.singleton aVariable
                                    bTypeNotVariable
                            , equivalentVariables = []
                            }
                        }


typeNotVariableUnify :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> TypeNotVariable TypeVariableFromContext
    -> TypeNotVariable TypeVariableFromContext
    ->
        Result
            String
            { type_ : Type TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeNotVariableUnify declarationTypes a b =
    -- (!!) TODO explicitly check for aliases on either side first
    case a of
        TypeUnit ->
            case b of
                TypeUnit ->
                    Ok
                        { type_ = TypeNotVariable TypeUnit
                        , substitutions = variableSubstitutionsNone
                        }

                _ ->
                    Err "unit (`()`) cannot be unified with types other than unit"

        TypeConstruct aTypeConstruct ->
            let
                maybeBTypeConstructWithSameNameArguments : Maybe (List (Type TypeVariableFromContext))
                maybeBTypeConstructWithSameNameArguments =
                    case b of
                        TypeConstruct bTypeConstruct ->
                            if
                                (aTypeConstruct.moduleOrigin
                                    == bTypeConstruct.moduleOrigin
                                )
                                    && (aTypeConstruct.name
                                            == bTypeConstruct.name
                                       )
                            then
                                Just bTypeConstruct.arguments

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            case maybeBTypeConstructWithSameNameArguments of
                Just bTypeConstructArguments ->
                    List.map2
                        (\aArgument bArgument -> { a = aArgument, b = bArgument })
                        aTypeConstruct.arguments
                        bTypeConstructArguments
                        |> listFoldlWhileOkFrom
                            { argumentsReverse = []
                            , substitutions = variableSubstitutionsNone
                            }
                            (\ab soFar ->
                                typeUnify declarationTypes ab.a ab.b
                                    |> Result.andThen
                                        (\argumentTypeUnifiedAndSubstitutions ->
                                            variableSubstitutionsMerge
                                                declarationTypes
                                                argumentTypeUnifiedAndSubstitutions.substitutions
                                                soFar.substitutions
                                                |> Result.map
                                                    (\substitutionsWithArgument ->
                                                        { argumentsReverse =
                                                            argumentTypeUnifiedAndSubstitutions.type_
                                                                :: soFar.argumentsReverse
                                                        , substitutions =
                                                            substitutionsWithArgument
                                                        }
                                                    )
                                        )
                            )
                        |> Result.map
                            (\substitutionsAndArgumentsUnified ->
                                { type_ =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = aTypeConstruct.moduleOrigin
                                            , name = aTypeConstruct.name
                                            , arguments =
                                                substitutionsAndArgumentsUnified.argumentsReverse
                                                    |> List.reverse
                                            }
                                        )
                                , substitutions = substitutionsAndArgumentsUnified.substitutions
                                }
                            )

                Nothing ->
                    -- TODO "expand alias left if possible and unify, then try right and unify.
                    -- if both are not in context, err
                    -- TODO also check locally declared
                    case declarationTypes |> FastDict.get aTypeConstruct.moduleOrigin of
                        Nothing ->
                            Err
                                ("no choice type/type alias declaration found for "
                                    ++ qualifiedToString { qualification = aTypeConstruct.moduleOrigin, name = aTypeConstruct.name }
                                )

                        Just aOriginModuleTypes ->
                            case aOriginModuleTypes.typeAliases |> FastDict.get aTypeConstruct.name of
                                Just originAliasDeclaration ->
                                    List.map2
                                        (\parameterName argument ->
                                            { variable = ( [], parameterName ), type_ = argument }
                                        )
                                        originAliasDeclaration.parameters
                                        aTypeConstruct.arguments
                                        |> listFoldlWhileOkFrom
                                            { type_ =
                                                originAliasDeclaration.type_
                                                    |> typeVariablesMap (\aliasVariable -> ( [], aliasVariable ))
                                            , substitutions = variableSubstitutionsNone
                                            }
                                            (\substitution soFar ->
                                                soFar.type_
                                                    |> typeSubstituteVariableBy declarationTypes
                                                        { variable = substitution.variable
                                                        , type_ = substitution.type_
                                                        }
                                                    |> Result.andThen
                                                        (\substituted ->
                                                            variableSubstitutionsMerge declarationTypes
                                                                substituted.substitutions
                                                                soFar.substitutions
                                                                |> Result.map
                                                                    (\substitutionsAfterSubstitution ->
                                                                        { type_ = substituted.type_
                                                                        , substitutions = substitutionsAfterSubstitution
                                                                        }
                                                                    )
                                                        )
                                            )

                                Nothing ->
                                    -- choice type
                                    -- TODO check right for alias
                                    Debug.todo ""

        TypeTuple aTuple ->
            case b of
                TypeTuple bTuple ->
                    Result.map2
                        (\part0ABUnified part1ABUnified ->
                            variableSubstitutionsMerge declarationTypes
                                part0ABUnified.substitutions
                                part1ABUnified.substitutions
                                |> Result.map
                                    (\substitutionsABMerged ->
                                        { type_ =
                                            TypeNotVariable
                                                (TypeTuple
                                                    { part0 = part0ABUnified.type_
                                                    , part1 = part1ABUnified.type_
                                                    }
                                                )
                                        , substitutions = substitutionsABMerged
                                        }
                                    )
                        )
                        (typeUnify declarationTypes aTuple.part0 bTuple.part0)
                        (typeUnify declarationTypes aTuple.part1 bTuple.part1)
                        |> Result.andThen identity

                _ ->
                    Err "tuple (`( ..., ... )`) cannot be unified with types other than tuple"

        TypeTriple aTriple ->
            case b of
                TypeTriple bTriple ->
                    Result.map3
                        (\part0ABUnified part1ABUnified part2ABUnified ->
                            variableSubstitutionsMerge3 declarationTypes
                                part0ABUnified.substitutions
                                part1ABUnified.substitutions
                                part2ABUnified.substitutions
                                |> Result.map
                                    (\substitutionsABMerged ->
                                        { type_ =
                                            TypeNotVariable
                                                (TypeTriple
                                                    { part0 = part0ABUnified.type_
                                                    , part1 = part1ABUnified.type_
                                                    , part2 = part2ABUnified.type_
                                                    }
                                                )
                                        , substitutions = substitutionsABMerged
                                        }
                                    )
                        )
                        (typeUnify declarationTypes aTriple.part0 bTriple.part0)
                        (typeUnify declarationTypes aTriple.part1 bTriple.part1)
                        (typeUnify declarationTypes aTriple.part1 bTriple.part1)
                        |> Result.andThen identity

                _ ->
                    Err "triple (`( ..., ..., ... )`) cannot be unified with types other than triple"

        TypeRecord aRecord ->
            case b of
                TypeRecord bRecord ->
                    typeRecordUnify declarationTypes aRecord bRecord
                        |> Result.map
                            (\typeAndSubstitutions ->
                                { type_ = TypeNotVariable typeAndSubstitutions.type_
                                , substitutions = typeAndSubstitutions.substitutions
                                }
                            )

                TypeRecordExtension bRecordExtension ->
                    typeRecordExtensionRecordUnify declarationTypes
                        bRecordExtension
                        aRecord
                        |> Result.map
                            (\typeAndSubstitutions ->
                                { type_ = TypeNotVariable typeAndSubstitutions.type_
                                , substitutions = typeAndSubstitutions.substitutions
                                }
                            )

                _ ->
                    Err "record cannot be unified with types other than record or record extension"

        TypeRecordExtension aRecordExtension ->
            -- TODO add fields and unify overlapping fields
            Debug.todo ""

        TypeFunction aFunction ->
            case b of
                TypeFunction bFunction ->
                    Result.map2
                        (\inputABUnified outputABUnified ->
                            variableSubstitutionsMerge declarationTypes
                                inputABUnified.substitutions
                                outputABUnified.substitutions
                                |> Result.map
                                    (\substitutionsABMerged ->
                                        { type_ =
                                            TypeNotVariable
                                                (TypeFunction
                                                    { input = inputABUnified.type_
                                                    , output = outputABUnified.type_
                                                    }
                                                )
                                        , substitutions = substitutionsABMerged
                                        }
                                    )
                        )
                        (typeUnify declarationTypes aFunction.input bFunction.input)
                        (typeUnify declarationTypes aFunction.output bFunction.output)
                        |> Result.andThen identity

                _ ->
                    Err "function (`... -> ...`) cannot be unified with types other than function"


typeRecordUnify :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> FastDict.Dict String (Type variable)
    -> FastDict.Dict String (Type variable)
    ->
        Result
            String
            { type_ : TypeNotVariable variable
            , substitutions : VariableSubstitutions
            }
typeRecordUnify declarationTypes aFields bFields =
    -- TODO unify overlapping fields
    Ok
        { type_ =
            TypeRecord
                (FastDict.union
                    aFields
                    bFields
                )
        , substitutions = variableSubstitutionsNone
        }


typeRecordExtensionRecordUnify :
    ModuleLevelDeclarationTypesInAvailableInModule
    ->
        { recordVariable : variable
        , fields : FastDict.Dict String (Type variable)
        }
    -> FastDict.Dict String (Type variable)
    ->
        Result
            String
            { type_ : TypeNotVariable variable
            , substitutions : VariableSubstitutions
            }
typeRecordExtensionRecordUnify declarationTypes recordExtension record =
    -- TODO unify overlapping fields
    Debug.todo ""


{-| A part in the syntax tree

  - its [range](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range#Range) in the source
  - its [`Type`](#Type)

-}
type alias TypedNode value =
    { range : Elm.Syntax.Range.Range
    , value : value
    , type_ : Type TypeVariableFromContext
    }


{-| Like [`Elm.Syntax.Expression.Expression`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Expression)
but all its sub-nodes are [`TypedNode`](#TypedNode)s
-}
type Expression
    = ExpressionUnit
    | ExpressionCall
        { called : TypedNode Expression
        , argument0 : TypedNode Expression
        , argument1Up : List (TypedNode Expression)
        }
    | ExpressionInfixOperation
        { symbol : String
        , left : TypedNode Expression
        , right : TypedNode Expression
        }
    | ExpressionReference
        { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
        , qualification : Elm.Syntax.ModuleName.ModuleName
        , name : String
        }
    | ExpressionIfThenElse
        { condition : TypedNode Expression
        , onTrue : TypedNode Expression
        , onFalse : TypedNode Expression
        }
    | ExpressionOperatorFunction String
    | ExpressionNumber
        { base : Base10Or16
        , value : Int
        }
    | ExpressionFloat Float
    | ExpressionNegation (TypedNode Expression)
    | ExpressionString String
    | ExpressionChar Char
    | ExpressionTuple
        { part0 : TypedNode Expression
        , part1 : TypedNode Expression
        }
    | ExpressionTriple
        { part0 : TypedNode Expression
        , part1 : TypedNode Expression
        , part2 : TypedNode Expression
        }
    | ExpressionParenthesized (TypedNode Expression)
    | ExpressionLetIn
        { declarations : List LetDeclaration
        , result : TypedNode Expression
        }
    | ExpressionCaseOf
        { matchedExpression : TypedNode Expression
        , cases :
            List
                { pattern : TypedNode Pattern
                , result : TypedNode Expression
                }
        }
    | ExpressionLambda
        { arguments : List (TypedNode Pattern)
        , result : TypedNode Expression
        }
    | ExpressionRecord
        (List
            { fieldRange : Elm.Syntax.Range.Range
            , name : String
            , nameRange : Elm.Syntax.Range.Range
            , value : TypedNode Expression
            }
        )
    | ExpressionList (List (TypedNode Expression))
    | ExpressionRecordAccess
        { record : TypedNode Expression
        , fieldNameRange : Elm.Syntax.Range.Range
        , fieldName : String
        }
    | ExpressionRecordAccessFunction String
    | ExpressionRecordUpdate
        { recordVariable : TypedNode String
        , fields :
            List
                { fieldRange : Elm.Syntax.Range.Range
                , name : String
                , nameRange : Elm.Syntax.Range.Range
                , value : TypedNode Expression
                }
        }


{-| Like [`Elm.Syntax.Expression.LetDeclaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#LetDeclaration)
but all its sub-nodes are [`TypedNode`](#TypedNode)s
-}
type LetDeclaration
    = LetDestructuring
        { pattern : TypedNode Pattern
        , expression : TypedNode Expression
        }
    | LetValueOrFunction
        { signature :
            { nameRange : Elm.Syntax.Range.Range
            , type_ : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
            }
        , nameRange : Elm.Syntax.Range.Range
        , name : String
        , arguments : List (TypedNode Pattern)
        , result : TypedNode Expression
        }


{-| Like [`Elm.Syntax.Pattern.Pattern`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern#Pattern)
but all its sub-nodes are [`TypedNode`](#TypedNode)s
-}
type Pattern
    = PatternIgnored
    | PatternVariable String
    | PatternParenthesized (TypedNode Pattern)
    | PatternAs
        { pattern : TypedNode Pattern
        , variable : TypedNode String
        }
    | PatternUnit
    | PatternChar Char
    | PatternString String
    | PatternInt { base : Base10Or16, value : Int }
    | PatternTuple
        { part0 : TypedNode Pattern
        , part1 : TypedNode Pattern
        }
    | PatternTriple
        { part0 : TypedNode Pattern
        , part1 : TypedNode Pattern
        , part2 : TypedNode Pattern
        }
    | PatternRecord (List (TypedNode String))
    | PatternListCons
        { head : TypedNode Pattern
        , tail : TypedNode Pattern
        }
    | PatternListExact (List (TypedNode Pattern))
    | PatternVariant
        { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
        , qualification : Elm.Syntax.ModuleName.ModuleName
        , name : String
        , arguments : List (TypedNode Pattern)
        }


{-| Either decimal or hexadecimal.
Used by integer pattern and expression
-}
type Base10Or16
    = Base10
    | Base16


typeBasicsFloat : Type variable_
typeBasicsFloat =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Basics" ]
            , name = "Float"
            , arguments = []
            }
        )


typeBasicsInt : Type variable_
typeBasicsInt =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Basics" ]
            , name = "Int"
            , arguments = []
            }
        )


typeStringString : Type variable_
typeStringString =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "String" ]
            , name = "String"
            , arguments = []
            }
        )


typeCharChar : Type variable_
typeCharChar =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Char" ]
            , name = "Char"
            , arguments = []
            }
        )


typeBasicsBool : Type variable_
typeBasicsBool =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Basics" ]
            , name = "Bool"
            , arguments = []
            }
        )


typeListList : Type variable -> Type variable
typeListList a =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "List" ]
            , name = "List"
            , arguments = [ a ]
            }
        )


typeParserParser : Type variable -> Type variable
typeParserParser a =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Parser" ]
            , name = "Parser"
            , arguments = [ a ]
            }
        )


typeParserAdvancedParser : Type variable -> Type variable -> Type variable -> Type variable
typeParserAdvancedParser context problem value =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Parser", "Advanced" ]
            , name = "Parser"
            , arguments = [ context, problem, value ]
            }
        )


typeUrlParserParser : Type variable -> Type variable -> Type variable
typeUrlParserParser a b =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Url", "Parser" ]
            , name = "Parser"
            , arguments = [ a, b ]
            }
        )


typeUrlParserQueryParser : Type variable -> Type variable
typeUrlParserQueryParser a =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Url", "Parser", "Query" ]
            , name = "Parser"
            , arguments = [ a ]
            }
        )


patternContextToInPath :
    String
    ->
        { path : List String
        , moduleOriginLookup : ModuleOriginLookup
        , declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
        }
    ->
        { path : List String
        , moduleOriginLookup : ModuleOriginLookup
        , declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
        }
patternContextToInPath innermostPathPart context =
    { path = innermostPathPart :: context.path
    , moduleOriginLookup = context.moduleOriginLookup
    , declarationTypes = context.declarationTypes
    }


patternTypeInfer :
    { path : List String
    , moduleOriginLookup : ModuleOriginLookup
    , declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { substitutions : VariableSubstitutions
            , node : TypedNode Pattern
            , introducedExpressionVariables :
                FastDict.Dict
                    String
                    (Type TypeVariableFromContext)
            }
patternTypeInfer context (Elm.Syntax.Node.Node fullRange pattern) =
    -- IGNORE TCO
    case pattern of
        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "float patterns are invalid syntax"

        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { node =
                    { range = fullRange
                    , value = PatternIgnored
                    , type_ = TypeVariable ( context.path, "ignored" )
                    }
                , substitutions = variableSubstitutionsNone
                , introducedExpressionVariables = FastDict.empty
                }

        Elm.Syntax.Pattern.ParenthesizedPattern parenthesizedInParens ->
            parenthesizedInParens
                |> patternTypeInfer context
                |> Result.map
                    (\inParens ->
                        { substitutions = inParens.substitutions
                        , node =
                            { type_ = inParens.node.type_
                            , value =
                                PatternParenthesized inParens.node
                            , range = fullRange
                            }
                        , introducedExpressionVariables = inParens.introducedExpressionVariables
                        }
                    )

        Elm.Syntax.Pattern.VarPattern variableName ->
            let
                type_ : Type TypeVariableFromContext
                type_ =
                    TypeVariable ( context.path, variableName )
            in
            Ok
                { substitutions = variableSubstitutionsNone
                , node =
                    { range = fullRange
                    , value = PatternVariable variableName
                    , type_ = type_
                    }
                , introducedExpressionVariables =
                    FastDict.singleton variableName
                        type_
                }

        Elm.Syntax.Pattern.AsPattern innerPatternNode (Elm.Syntax.Node.Node variableNameRange variableName) ->
            innerPatternNode
                |> patternTypeInfer context
                |> Result.map
                    (\inner ->
                        { substitutions = inner.substitutions
                        , node =
                            { type_ = inner.node.type_
                            , value =
                                PatternAs
                                    { pattern = inner.node
                                    , variable =
                                        { value = variableName
                                        , range = variableNameRange
                                        , type_ = inner.node.type_
                                        }
                                    }
                            , range = fullRange
                            }
                        , introducedExpressionVariables = inner.introducedExpressionVariables
                        }
                    )

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { node =
                    { range = fullRange
                    , value = PatternUnit
                    , type_ = TypeNotVariable TypeUnit
                    }
                , substitutions = variableSubstitutionsNone
                , introducedExpressionVariables = FastDict.empty
                }

        Elm.Syntax.Pattern.CharPattern charValue ->
            Ok
                { node =
                    { range = fullRange
                    , value = PatternChar charValue
                    , type_ = typeCharChar
                    }
                , substitutions = variableSubstitutionsNone
                , introducedExpressionVariables = FastDict.empty
                }

        Elm.Syntax.Pattern.StringPattern stringValue ->
            Ok
                { node =
                    { range = fullRange
                    , value = PatternString stringValue
                    , type_ = typeStringString
                    }
                , substitutions = variableSubstitutionsNone
                , introducedExpressionVariables = FastDict.empty
                }

        Elm.Syntax.Pattern.IntPattern intValue ->
            Ok
                { node =
                    { range = fullRange
                    , value = PatternInt { base = Base10, value = intValue }
                    , type_ = typeBasicsInt
                    }
                , substitutions = variableSubstitutionsNone
                , introducedExpressionVariables = FastDict.empty
                }

        Elm.Syntax.Pattern.HexPattern intValue ->
            Ok
                { node =
                    { range = fullRange
                    , value = PatternInt { base = Base16, value = intValue }
                    , type_ = typeBasicsInt
                    }
                , substitutions = variableSubstitutionsNone
                , introducedExpressionVariables = FastDict.empty
                }

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [] ->
                    -- should be handled by UnitPattern
                    Ok
                        { node =
                            { range = fullRange
                            , value = PatternUnit
                            , type_ = TypeNotVariable TypeUnit
                            }
                        , substitutions = variableSubstitutionsNone
                        , introducedExpressionVariables = FastDict.empty
                        }

                [ parenthesizedInParens ] ->
                    -- should be handled by ParenthesizedPattern
                    parenthesizedInParens
                        |> patternTypeInfer context
                        |> Result.map
                            (\inParens ->
                                { substitutions = inParens.substitutions
                                , node =
                                    { type_ = inParens.node.type_
                                    , value =
                                        PatternParenthesized inParens.node
                                    , range = fullRange
                                    }
                                , introducedExpressionVariables = inParens.introducedExpressionVariables
                                }
                            )

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            variableSubstitutionsMerge
                                context.declarationTypes
                                part0.substitutions
                                part1.substitutions
                                |> Result.map
                                    (\substitutionsPart01 ->
                                        { node =
                                            { range = fullRange
                                            , value = PatternTuple { part0 = part0.node, part1 = part1.node }
                                            , type_ =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 = part0.node.type_, part1 = part1.node.type_ }
                                                    )
                                            }
                                        , substitutions = substitutionsPart01
                                        , introducedExpressionVariables =
                                            FastDict.union
                                                part0.introducedExpressionVariables
                                                part1.introducedExpressionVariables
                                        }
                                    )
                        )
                        (tuplePart0 |> patternTypeInfer context)
                        (tuplePart1 |> patternTypeInfer context)
                        |> Result.andThen identity

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            variableSubstitutionsMerge3
                                context.declarationTypes
                                part0.substitutions
                                part1.substitutions
                                part2.substitutions
                                |> Result.map
                                    (\substitutionsPart012 ->
                                        { node =
                                            { range = fullRange
                                            , value =
                                                PatternTriple
                                                    { part0 = part0.node
                                                    , part1 = part1.node
                                                    , part2 = part2.node
                                                    }
                                            , type_ =
                                                TypeNotVariable
                                                    (TypeTriple
                                                        { part0 = part0.node.type_
                                                        , part1 = part1.node.type_
                                                        , part2 = part2.node.type_
                                                        }
                                                    )
                                            }
                                        , substitutions = substitutionsPart012
                                        , introducedExpressionVariables =
                                            FastDict.union
                                                part0.introducedExpressionVariables
                                                (FastDict.union
                                                    part1.introducedExpressionVariables
                                                    part2.introducedExpressionVariables
                                                )
                                        }
                                    )
                        )
                        (tuplePart0 |> patternTypeInfer context)
                        (tuplePart1 |> patternTypeInfer context)
                        (tuplePart2 |> patternTypeInfer context)
                        |> Result.andThen identity

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.Pattern.RecordPattern recordFields ->
            let
                fieldTypedNodes : List (TypedNode String)
                fieldTypedNodes =
                    recordFields
                        |> List.map
                            (\(Elm.Syntax.Node.Node fieldRange fieldName) ->
                                { range = fieldRange
                                , value = fieldName
                                , type_ = TypeVariable ( context.path, fieldName )
                                }
                            )
            in
            Ok
                { node =
                    { range = fullRange
                    , value = PatternRecord fieldTypedNodes
                    , type_ =
                        TypeNotVariable
                            (TypeRecord
                                (fieldTypedNodes
                                    |> List.foldl
                                        (\fieldVariable soFar ->
                                            soFar
                                                |> FastDict.insert fieldVariable.value
                                                    fieldVariable.type_
                                        )
                                        FastDict.empty
                                )
                            )
                    }
                , substitutions = variableSubstitutionsNone
                , introducedExpressionVariables =
                    fieldTypedNodes
                        |> List.foldl
                            (\fieldVariableTypedNode soFar ->
                                soFar
                                    |> FastDict.insert fieldVariableTypedNode.value
                                        fieldVariableTypedNode.type_
                            )
                            FastDict.empty
                }

        Elm.Syntax.Pattern.UnConsPattern headNode tailNode ->
            Result.map2
                (\headInferred tailInferred ->
                    typeUnify
                        context.declarationTypes
                        headInferred.node.type_
                        tailInferred.node.type_
                        |> Result.andThen
                            (\fullListUnified ->
                                variableSubstitutionsMerge3
                                    context.declarationTypes
                                    headInferred.substitutions
                                    tailInferred.substitutions
                                    fullListUnified.substitutions
                                    |> Result.map
                                        (\substitutionsHeadTail ->
                                            { substitutions = substitutionsHeadTail
                                            , introducedExpressionVariables =
                                                FastDict.union
                                                    headInferred.introducedExpressionVariables
                                                    tailInferred.introducedExpressionVariables
                                            , node =
                                                { range = fullRange
                                                , value =
                                                    PatternListCons
                                                        { head = headInferred.node
                                                        , tail =
                                                            { range = tailInferred.node.range
                                                            , value = tailInferred.node.value
                                                            , type_ = fullListUnified.type_
                                                            }
                                                        }
                                                , type_ = fullListUnified.type_
                                                }
                                            }
                                        )
                            )
                )
                (patternTypeInfer
                    (context |> patternContextToInPath "head")
                    headNode
                )
                (patternTypeInfer
                    (context |> patternContextToInPath "tail")
                    tailNode
                )
                |> Result.andThen identity

        Elm.Syntax.Pattern.ListPattern elementNodes ->
            case elementNodes of
                [] ->
                    Ok
                        { substitutions = variableSubstitutionsNone
                        , introducedExpressionVariables = FastDict.empty
                        , node =
                            { range = fullRange
                            , value = PatternListExact []
                            , type_ =
                                typeListList
                                    (TypeVariable ( context.path, "element" ))
                            }
                        }

                head :: tail ->
                    patternTypeInfer
                        (context |> patternContextToInPath "0")
                        head
                        |> Result.andThen
                            (\headTypedNodeAndSubstitutions ->
                                tail
                                    |> listFoldlWhileOkFrom
                                        { substitutions = headTypedNodeAndSubstitutions.substitutions
                                        , elementType = headTypedNodeAndSubstitutions.node.type_
                                        , elementNodesReverse = []
                                        , introducedExpressionVariables = FastDict.empty
                                        , index = 1
                                        }
                                        (\elementNode soFar ->
                                            patternTypeInfer
                                                (context
                                                    |> patternContextToInPath
                                                        (soFar.index |> String.fromInt)
                                                )
                                                elementNode
                                                |> Result.andThen
                                                    (\elementTypedNodeAndSubstitutions ->
                                                        typeUnify context.declarationTypes
                                                            elementTypedNodeAndSubstitutions.node.type_
                                                            soFar.elementType
                                                            |> Result.andThen
                                                                (\elementTypeWithCurrent ->
                                                                    variableSubstitutionsMerge3 context.declarationTypes
                                                                        elementTypedNodeAndSubstitutions.substitutions
                                                                        elementTypeWithCurrent.substitutions
                                                                        soFar.substitutions
                                                                        |> Result.map
                                                                            (\substitutionsWithElement ->
                                                                                { index = soFar.index + 1
                                                                                , elementNodesReverse =
                                                                                    { range = elementTypedNodeAndSubstitutions.node.range
                                                                                    , value = elementTypedNodeAndSubstitutions.node.value
                                                                                    }
                                                                                        :: soFar.elementNodesReverse
                                                                                , elementType = elementTypeWithCurrent.type_
                                                                                , substitutions = substitutionsWithElement
                                                                                , introducedExpressionVariables =
                                                                                    elementTypedNodeAndSubstitutions.introducedExpressionVariables
                                                                                }
                                                                            )
                                                                )
                                                    )
                                        )
                            )
                        |> Result.map
                            (\elementTypeAndSubstitutions ->
                                { substitutions = elementTypeAndSubstitutions.substitutions
                                , introducedExpressionVariables =
                                    elementTypeAndSubstitutions.introducedExpressionVariables
                                , node =
                                    { range = fullRange
                                    , value =
                                        PatternListExact
                                            (elementTypeAndSubstitutions.elementNodesReverse
                                                |> listReverseAndMap
                                                    (\elementNode ->
                                                        { range = elementNode.range
                                                        , value = elementNode.value
                                                        , type_ = elementTypeAndSubstitutions.elementType
                                                        }
                                                    )
                                            )
                                    , type_ =
                                        typeListList elementTypeAndSubstitutions.elementType
                                    }
                                }
                            )

        Elm.Syntax.Pattern.NamedPattern _ _ ->
            Debug.todo ""


expressionTypeInfer :
    { declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
    , locallyIntroducedVariableExpressions :
        FastDict.Dict String (Type TypeVariableFromContext)
    , path : List String
    , moduleOriginLookup : ModuleOriginLookup
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        Result
            String
            { substitutions : VariableSubstitutions
            , node : TypedNode Expression
            }
expressionTypeInfer context (Elm.Syntax.Node.Node fullRange expression) =
    -- IGNORE TCO
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node =
                    { range = fullRange
                    , value = ExpressionUnit
                    , type_ = TypeNotVariable TypeUnit
                    }
                }

        Elm.Syntax.Expression.Application _ ->
            Debug.todo "branch 'Application _' not implemented"

        Elm.Syntax.Expression.OperatorApplication _ _ _ _ ->
            Debug.todo "branch 'OperatorApplication _ _ _ _' not implemented"

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            -- TODO check for locallyIntroducedVariableExpressions first
            Debug.todo "branch 'FunctionOrValue _ _' not implemented"

        Elm.Syntax.Expression.IfBlock _ _ _ ->
            Debug.todo "branch 'unify condition with Basics.Bool"

        Elm.Syntax.Expression.PrefixOperator operator ->
            operatorFunctionType
                { path = context.path
                , moduleOriginLookup = context.moduleOriginLookup
                }
                operator
                |> Result.map
                    (\type_ ->
                        { node =
                            { range = fullRange
                            , value = ExpressionOperatorFunction operator
                            , type_ = type_
                            }
                        , substitutions = variableSubstitutionsNone
                        }
                    )

        Elm.Syntax.Expression.Operator _ ->
            Err "Elm.Syntax.Expression.Operator should not exist in a valid parse result"

        Elm.Syntax.Expression.Integer intValue ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node =
                    { range = fullRange
                    , value = ExpressionNumber { base = Base10, value = intValue }
                    , type_ = TypeVariable ( context.path, "number" )
                    }
                }

        Elm.Syntax.Expression.Hex intValue ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node =
                    { range = fullRange
                    , value = ExpressionNumber { base = Base16, value = intValue }
                    , type_ = TypeVariable ( context.path, "number" )
                    }
                }

        Elm.Syntax.Expression.Floatable floatValue ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node =
                    { range = fullRange
                    , value = ExpressionFloat floatValue
                    , type_ = typeBasicsFloat
                    }
                }

        Elm.Syntax.Expression.Negation _ ->
            Debug.todo "branch 'Negation _' not implemented"

        Elm.Syntax.Expression.Literal stringValue ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node =
                    { range = fullRange
                    , value = ExpressionString stringValue
                    , type_ = typeStringString
                    }
                }

        Elm.Syntax.Expression.CharLiteral charValue ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node =
                    { range = fullRange
                    , value = ExpressionChar charValue
                    , type_ = typeCharChar
                    }
                }

        Elm.Syntax.Expression.TupledExpression tupleParts ->
            case tupleParts of
                [] ->
                    -- should be handled by UnitExpr
                    Ok
                        { substitutions = variableSubstitutionsNone
                        , node =
                            { range = fullRange
                            , value = ExpressionUnit
                            , type_ = TypeNotVariable TypeUnit
                            }
                        }

                [ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    inParens
                        |> expressionTypeInfer context
                        |> Result.map
                            (\inParensNodeAndSubstitutions ->
                                { node =
                                    { value = ExpressionParenthesized inParensNodeAndSubstitutions.node
                                    , type_ = inParensNodeAndSubstitutions.node.type_
                                    , range = fullRange
                                    }
                                , substitutions = inParensNodeAndSubstitutions.substitutions
                                }
                            )

                [ part0, part1 ] ->
                    Result.map2
                        (\part0NodeAndSubstitutions part1NodeAndSubstitutions ->
                            variableSubstitutionsMerge context.declarationTypes
                                part0NodeAndSubstitutions.substitutions
                                part1NodeAndSubstitutions.substitutions
                                |> Result.map
                                    (\substitutions ->
                                        { substitutions = substitutions
                                        , node =
                                            { range = fullRange
                                            , value =
                                                ExpressionTuple
                                                    { part0 = part0NodeAndSubstitutions.node
                                                    , part1 = part1NodeAndSubstitutions.node
                                                    }
                                            , type_ =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 = part0NodeAndSubstitutions.node.type_
                                                        , part1 = part1NodeAndSubstitutions.node.type_
                                                        }
                                                    )
                                            }
                                        }
                                    )
                        )
                        (part0 |> expressionTypeInfer (context |> expressionContextToInPath "first"))
                        (part1 |> expressionTypeInfer (context |> expressionContextToInPath "second"))
                        |> Result.andThen identity

                [ part0, part1, part2 ] ->
                    Result.map3
                        (\part0NodeAndSubstitutions part1NodeAndSubstitutions part2NodeAndSubstitutions ->
                            variableSubstitutionsMerge3 context.declarationTypes
                                part0NodeAndSubstitutions.substitutions
                                part1NodeAndSubstitutions.substitutions
                                part2NodeAndSubstitutions.substitutions
                                |> Result.map
                                    (\substitutions ->
                                        { substitutions = substitutions
                                        , node =
                                            { range = fullRange
                                            , value =
                                                ExpressionTriple
                                                    { part0 = part0NodeAndSubstitutions.node
                                                    , part1 = part1NodeAndSubstitutions.node
                                                    , part2 = part2NodeAndSubstitutions.node
                                                    }
                                            , type_ =
                                                TypeNotVariable
                                                    (TypeTriple
                                                        { part0 = part0NodeAndSubstitutions.node.type_
                                                        , part1 = part1NodeAndSubstitutions.node.type_
                                                        , part2 = part2NodeAndSubstitutions.node.type_
                                                        }
                                                    )
                                            }
                                        }
                                    )
                        )
                        (part0 |> expressionTypeInfer (context |> expressionContextToInPath "first"))
                        (part1 |> expressionTypeInfer (context |> expressionContextToInPath "second"))
                        (part2 |> expressionTypeInfer (context |> expressionContextToInPath "third"))
                        |> Result.andThen identity

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts. Should not exist in a valid parse result"

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens
                |> expressionTypeInfer context
                |> Result.map
                    (\inParensNodeAndSubstitutions ->
                        { node =
                            { value = ExpressionParenthesized inParensNodeAndSubstitutions.node
                            , type_ = inParensNodeAndSubstitutions.node.type_
                            , range = fullRange
                            }
                        , substitutions = inParensNodeAndSubstitutions.substitutions
                        }
                    )

        Elm.Syntax.Expression.LetExpression letIn ->
            Debug.todo "branch 'LetExpression _' not implemented"

        Elm.Syntax.Expression.CaseExpression caseOf ->
            Debug.todo "branch 'CaseExpression _' not implemented"

        Elm.Syntax.Expression.LambdaExpression lambda ->
            Debug.todo "branch 'LambdaExpression _' not implemented"

        Elm.Syntax.Expression.RecordExpr fields ->
            fields
                |> listFoldlWhileOkFrom
                    { substitutions = variableSubstitutionsNone
                    , fieldTypedNodesReverse = []
                    }
                    (\(Elm.Syntax.Node.Node fieldRange ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )) soFar ->
                        fieldValueNode
                            |> expressionTypeInfer
                                (context |> expressionContextToInPath fieldName)
                            |> Result.andThen
                                (\fieldValueTypedNode ->
                                    variableSubstitutionsMerge context.declarationTypes
                                        fieldValueTypedNode.substitutions
                                        soFar.substitutions
                                        |> Result.map
                                            (\substitutionsWithField ->
                                                { fieldTypedNodesReverse =
                                                    { fieldRange = fieldRange
                                                    , name = fieldName
                                                    , nameRange = fieldNameRange
                                                    , value = fieldValueTypedNode.node
                                                    }
                                                        :: soFar.fieldTypedNodesReverse
                                                , substitutions = substitutionsWithField
                                                }
                                            )
                                )
                    )
                |> Result.map
                    (\fieldTypedNodesAndSubstitutions ->
                        { substitutions =
                            fieldTypedNodesAndSubstitutions.substitutions
                        , node =
                            { range = fullRange
                            , value =
                                ExpressionRecord
                                    (fieldTypedNodesAndSubstitutions.fieldTypedNodesReverse
                                        |> List.reverse
                                    )
                            , type_ =
                                TypeNotVariable
                                    (TypeRecord
                                        (fieldTypedNodesAndSubstitutions.fieldTypedNodesReverse
                                            |> List.foldl
                                                (\field soFar ->
                                                    soFar |> FastDict.insert field.name field.value.type_
                                                )
                                                FastDict.empty
                                        )
                                    )
                            }
                        }
                    )

        Elm.Syntax.Expression.ListExpr elements ->
            case elements of
                [] ->
                    Ok
                        { substitutions = variableSubstitutionsNone
                        , node =
                            { range = fullRange
                            , value = ExpressionList []
                            , type_ =
                                typeListList
                                    (TypeVariable ( context.path, "element" ))
                            }
                        }

                head :: tail ->
                    expressionTypeInfer
                        (context |> expressionContextToInPath "0")
                        head
                        |> Result.andThen
                            (\headTypedNodeAndSubstitutions ->
                                tail
                                    |> listFoldlWhileOkFrom
                                        { substitutions = headTypedNodeAndSubstitutions.substitutions
                                        , elementType = headTypedNodeAndSubstitutions.node.type_
                                        , elementNodesReverse = []
                                        , index = 1
                                        }
                                        (\elementNode soFar ->
                                            expressionTypeInfer
                                                (context
                                                    |> expressionContextToInPath
                                                        (soFar.index |> String.fromInt)
                                                )
                                                elementNode
                                                |> Result.andThen
                                                    (\elementTypedNodeAndSubstitutions ->
                                                        typeUnify context.declarationTypes
                                                            elementTypedNodeAndSubstitutions.node.type_
                                                            soFar.elementType
                                                            |> Result.andThen
                                                                (\elementTypeWithCurrent ->
                                                                    variableSubstitutionsMerge3 context.declarationTypes
                                                                        elementTypedNodeAndSubstitutions.substitutions
                                                                        elementTypeWithCurrent.substitutions
                                                                        soFar.substitutions
                                                                        |> Result.map
                                                                            (\substitutionsWithElement ->
                                                                                { index = soFar.index + 1
                                                                                , elementNodesReverse =
                                                                                    { range = elementTypedNodeAndSubstitutions.node.range
                                                                                    , value = elementTypedNodeAndSubstitutions.node.value
                                                                                    }
                                                                                        :: soFar.elementNodesReverse
                                                                                , elementType = elementTypeWithCurrent.type_
                                                                                , substitutions = substitutionsWithElement
                                                                                }
                                                                            )
                                                                )
                                                    )
                                        )
                            )
                        |> Result.map
                            (\elementTypeAndSubstitutions ->
                                { substitutions = elementTypeAndSubstitutions.substitutions
                                , node =
                                    { range = fullRange
                                    , value =
                                        ExpressionList
                                            (elementTypeAndSubstitutions.elementNodesReverse
                                                |> listReverseAndMap
                                                    (\elementNode ->
                                                        { range = elementNode.range
                                                        , value = elementNode.value
                                                        , type_ = elementTypeAndSubstitutions.elementType
                                                        }
                                                    )
                                            )
                                    , type_ =
                                        typeListList elementTypeAndSubstitutions.elementType
                                    }
                                }
                            )

        Elm.Syntax.Expression.RecordAccess recordNode fieldNameNode ->
            expressionTypeInfer context recordNode
                |> Result.andThen
                    (\recordTypedNodeAndSubstitutions ->
                        let
                            fieldName : String
                            fieldName =
                                fieldNameNode |> Elm.Syntax.Node.value

                            fieldValueType : Type TypeVariableFromContext
                            fieldValueType =
                                TypeVariable ( context.path, fieldName )
                        in
                        typeUnify context.declarationTypes
                            recordTypedNodeAndSubstitutions.node.type_
                            (TypeNotVariable
                                (TypeRecordExtension
                                    { recordVariable =
                                        ( context.path
                                        , "recordWith"
                                            ++ (fieldName |> stringFirstCharToUpper)
                                        )
                                    , fields =
                                        FastDict.singleton fieldName
                                            fieldValueType
                                    }
                                )
                            )
                            |> Result.andThen
                                (\recordWithAccessedField ->
                                    variableSubstitutionsMerge context.declarationTypes
                                        recordTypedNodeAndSubstitutions.substitutions
                                        recordWithAccessedField.substitutions
                                        |> Result.map
                                            (\fullSubstitutions ->
                                                { node =
                                                    { range = fullRange
                                                    , value =
                                                        ExpressionRecordAccess
                                                            { record =
                                                                { range = recordTypedNodeAndSubstitutions.node.range
                                                                , value = recordTypedNodeAndSubstitutions.node.value
                                                                , type_ = recordWithAccessedField.type_
                                                                }
                                                            , fieldName = fieldName
                                                            , fieldNameRange = fieldNameNode |> Elm.Syntax.Node.range
                                                            }
                                                    , type_ = fieldValueType
                                                    }
                                                , substitutions = fullSubstitutions
                                                }
                                            )
                                )
                    )

        Elm.Syntax.Expression.RecordAccessFunction dotFieldName ->
            let
                fieldName : String
                fieldName =
                    dotFieldName |> String.replace "." ""

                fieldValueType : Type TypeVariableFromContext
                fieldValueType =
                    TypeVariable ( context.path, fieldName )
            in
            Ok
                { node =
                    { range = fullRange
                    , value =
                        ExpressionRecordAccessFunction fieldName
                    , type_ =
                        TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeRecordExtension
                                            { recordVariable =
                                                ( context.path
                                                , "recordWith"
                                                    ++ (fieldName
                                                            |> stringFirstCharToUpper
                                                       )
                                                )
                                            , fields =
                                                FastDict.singleton fieldName
                                                    fieldValueType
                                            }
                                        )
                                , output = fieldValueType
                                }
                            )
                    }
                , substitutions = variableSubstitutionsNone
                }

        Elm.Syntax.Expression.RecordUpdateExpression recordVariable fields ->
            Debug.todo "branch 'RecordUpdateExpression _ _' not implemented"

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl shader expressions not supported"


listReverseAndMap : (a -> b) -> List a -> List b
listReverseAndMap elementChange list =
    list
        |> List.foldl
            (\element soFar -> (element |> elementChange) :: soFar)
            []


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toLower headChar) tailString


operatorFunctionType :
    { path : List String, moduleOriginLookup : ModuleOriginLookup }
    -> String
    -> Result String (Type TypeVariableFromContext)
operatorFunctionType context operator =
    case operator of
        "|>" ->
            let
                a : Type TypeVariableFromContext
                a =
                    TypeVariable ( context.path, "a" )

                b : Type TypeVariableFromContext
                b =
                    TypeVariable ( context.path, "b" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = a
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input =
                                        TypeNotVariable
                                            (TypeFunction
                                                { input = a
                                                , output = b
                                                }
                                            )
                                    , output = b
                                    }
                                )
                        }
                    )
                )

        "<|" ->
            let
                a : Type TypeVariableFromContext
                a =
                    TypeVariable ( context.path, "a" )

                b : Type TypeVariableFromContext
                b =
                    TypeVariable ( context.path, "b" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input =
                            TypeNotVariable
                                (TypeFunction
                                    { input = a
                                    , output = b
                                    }
                                )
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = a
                                    , output = b
                                    }
                                )
                        }
                    )
                )

        ">>" ->
            let
                a : Type TypeVariableFromContext
                a =
                    TypeVariable ( context.path, "a" )

                b : Type TypeVariableFromContext
                b =
                    TypeVariable ( context.path, "b" )

                c : Type TypeVariableFromContext
                c =
                    TypeVariable ( context.path, "c" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input =
                            TypeNotVariable
                                (TypeFunction
                                    { input = a
                                    , output = b
                                    }
                                )
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input =
                                        TypeNotVariable
                                            (TypeFunction
                                                { input = b
                                                , output = c
                                                }
                                            )
                                    , output =
                                        TypeNotVariable
                                            (TypeFunction
                                                { input = a
                                                , output = c
                                                }
                                            )
                                    }
                                )
                        }
                    )
                )

        "<<" ->
            let
                a : Type TypeVariableFromContext
                a =
                    TypeVariable ( context.path, "a" )

                b : Type TypeVariableFromContext
                b =
                    TypeVariable ( context.path, "b" )

                c : Type TypeVariableFromContext
                c =
                    TypeVariable ( context.path, "c" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input =
                            TypeNotVariable
                                (TypeFunction
                                    { input = b
                                    , output = c
                                    }
                                )
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input =
                                        TypeNotVariable
                                            (TypeFunction
                                                { input = a
                                                , output = b
                                                }
                                            )
                                    , output =
                                        TypeNotVariable
                                            (TypeFunction
                                                { input = a
                                                , output = c
                                                }
                                            )
                                    }
                                )
                        }
                    )
                )

        "++" ->
            let
                appendable : Type TypeVariableFromContext
                appendable =
                    TypeVariable ( context.path, "appendable" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = appendable
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = appendable
                                    , output = appendable
                                    }
                                )
                        }
                    )
                )

        "==" ->
            let
                equatable : Type TypeVariableFromContext
                equatable =
                    TypeVariable ( context.path, "equatable" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = equatable
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = equatable
                                    , output = typeBasicsBool
                                    }
                                )
                        }
                    )
                )

        "/=" ->
            let
                equatable : Type TypeVariableFromContext
                equatable =
                    TypeVariable ( context.path, "equatable" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = equatable
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = equatable
                                    , output = typeBasicsBool
                                    }
                                )
                        }
                    )
                )

        "::" ->
            let
                a : Type TypeVariableFromContext
                a =
                    TypeVariable ( context.path, "a" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = a
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = typeListList a
                                    , output = a
                                    }
                                )
                        }
                    )
                )

        "*" ->
            let
                number : Type TypeVariableFromContext
                number =
                    TypeVariable ( context.path, "number" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = number
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = number
                                    , output = number
                                    }
                                )
                        }
                    )
                )

        "+" ->
            let
                number : Type TypeVariableFromContext
                number =
                    TypeVariable ( context.path, "number" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = number
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = number
                                    , output = number
                                    }
                                )
                        }
                    )
                )

        "-" ->
            let
                number : Type TypeVariableFromContext
                number =
                    TypeVariable ( context.path, "number" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = number
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = number
                                    , output = number
                                    }
                                )
                        }
                    )
                )

        "/" ->
            let
                number : Type TypeVariableFromContext
                number =
                    TypeVariable ( context.path, "number" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = number
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = number
                                    , output = number
                                    }
                                )
                        }
                    )
                )

        "^" ->
            let
                number : Type TypeVariableFromContext
                number =
                    TypeVariable ( context.path, "number" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = number
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = number
                                    , output = number
                                    }
                                )
                        }
                    )
                )

        "<=" ->
            let
                comparable : Type TypeVariableFromContext
                comparable =
                    TypeVariable ( context.path, "comparable" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = comparable
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = comparable
                                    , output = typeBasicsBool
                                    }
                                )
                        }
                    )
                )

        ">=" ->
            let
                comparable : Type TypeVariableFromContext
                comparable =
                    TypeVariable ( context.path, "comparable" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = comparable
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = comparable
                                    , output = typeBasicsBool
                                    }
                                )
                        }
                    )
                )

        ">" ->
            let
                comparable : Type TypeVariableFromContext
                comparable =
                    TypeVariable ( context.path, "comparable" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = comparable
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = comparable
                                    , output = typeBasicsBool
                                    }
                                )
                        }
                    )
                )

        "<" ->
            let
                comparable : Type TypeVariableFromContext
                comparable =
                    TypeVariable ( context.path, "comparable" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = comparable
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = comparable
                                    , output = typeBasicsBool
                                    }
                                )
                        }
                    )
                )

        "//" ->
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = typeBasicsInt
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = typeBasicsInt
                                    , output = typeBasicsInt
                                    }
                                )
                        }
                    )
                )

        "&&" ->
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = typeBasicsBool
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = typeBasicsBool
                                    , output = typeBasicsBool
                                    }
                                )
                        }
                    )
                )

        "||" ->
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = typeBasicsBool
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = typeBasicsBool
                                    , output = typeBasicsBool
                                    }
                                )
                        }
                    )
                )

        "|." ->
            Ok
                (if context.moduleOriginLookup.ignoreOperatorIsExposedFromParserAdvanced then
                    let
                        varContext : Type TypeVariableFromContext
                        varContext =
                            TypeVariable ( context.path, "context" )

                        problem : Type TypeVariableFromContext
                        problem =
                            TypeVariable ( context.path, "problem" )

                        keep : Type TypeVariableFromContext
                        keep =
                            TypeVariable ( context.path, "keep" )

                        ignore : Type TypeVariableFromContext
                        ignore =
                            TypeVariable ( context.path, "ignore" )
                    in
                    TypeNotVariable
                        (TypeFunction
                            { input = typeParserAdvancedParser varContext problem keep
                            , output =
                                TypeNotVariable
                                    (TypeFunction
                                        { input = typeParserAdvancedParser varContext problem ignore
                                        , output = typeParserAdvancedParser varContext problem keep
                                        }
                                    )
                            }
                        )

                 else
                    let
                        keep : Type TypeVariableFromContext
                        keep =
                            TypeVariable ( context.path, "keep" )

                        ignore : Type TypeVariableFromContext
                        ignore =
                            TypeVariable ( context.path, "ignore" )
                    in
                    TypeNotVariable
                        (TypeFunction
                            { input = typeParserParser keep
                            , output =
                                TypeNotVariable
                                    (TypeFunction
                                        { input = typeParserParser ignore
                                        , output = typeParserParser keep
                                        }
                                    )
                            }
                        )
                )

        "|=" ->
            Ok
                (if context.moduleOriginLookup.keepOperatorIsExposedFromParserAdvanced then
                    let
                        varContext : Type TypeVariableFromContext
                        varContext =
                            TypeVariable ( context.path, "context" )

                        problem : Type TypeVariableFromContext
                        problem =
                            TypeVariable ( context.path, "problem" )

                        a : Type TypeVariableFromContext
                        a =
                            TypeVariable ( context.path, "keep" )

                        b : Type TypeVariableFromContext
                        b =
                            TypeVariable ( context.path, "ignore" )
                    in
                    TypeNotVariable
                        (TypeFunction
                            { input =
                                typeParserAdvancedParser
                                    varContext
                                    problem
                                    (TypeNotVariable
                                        (TypeFunction
                                            { input = a
                                            , output = b
                                            }
                                        )
                                    )
                            , output =
                                TypeNotVariable
                                    (TypeFunction
                                        { input = typeParserAdvancedParser varContext problem a
                                        , output = typeParserAdvancedParser varContext problem b
                                        }
                                    )
                            }
                        )

                 else
                    let
                        a : Type TypeVariableFromContext
                        a =
                            TypeVariable ( context.path, "keep" )

                        b : Type TypeVariableFromContext
                        b =
                            TypeVariable ( context.path, "ignore" )
                    in
                    TypeNotVariable
                        (TypeFunction
                            { input =
                                typeParserParser
                                    (TypeNotVariable
                                        (TypeFunction
                                            { input = a
                                            , output = b
                                            }
                                        )
                                    )
                            , output =
                                TypeNotVariable
                                    (TypeFunction
                                        { input = typeParserParser a
                                        , output = typeParserParser b
                                        }
                                    )
                            }
                        )
                )

        "</>" ->
            let
                a : Type TypeVariableFromContext
                a =
                    TypeVariable ( context.path, "a" )

                b : Type TypeVariableFromContext
                b =
                    TypeVariable ( context.path, "b" )

                c : Type TypeVariableFromContext
                c =
                    TypeVariable ( context.path, "c" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input = typeUrlParserParser a b
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = typeUrlParserParser b c
                                    , output = typeUrlParserParser a c
                                    }
                                )
                        }
                    )
                )

        "<?>" ->
            let
                a : Type TypeVariableFromContext
                a =
                    TypeVariable ( context.path, "a" )

                b : Type TypeVariableFromContext
                b =
                    TypeVariable ( context.path, "b" )

                query : Type TypeVariableFromContext
                query =
                    TypeVariable ( context.path, "query" )
            in
            Ok
                (TypeNotVariable
                    (TypeFunction
                        { input =
                            typeUrlParserParser
                                a
                                (TypeNotVariable
                                    (TypeFunction
                                        { input = query
                                        , output = b
                                        }
                                    )
                                )
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = typeUrlParserQueryParser query
                                    , output = typeUrlParserParser a b
                                    }
                                )
                        }
                    )
                )

        unknownOperator ->
            Err ("unknown operator (" ++ unknownOperator ++ ")")


expressionContextToInPath :
    String
    ->
        { declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
        , locallyIntroducedVariableExpressions :
            FastDict.Dict String (Type TypeVariableFromContext)
        , path : List String
        , moduleOriginLookup : ModuleOriginLookup
        }
    ->
        { declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
        , locallyIntroducedVariableExpressions :
            FastDict.Dict String (Type TypeVariableFromContext)
        , path : List String
        , moduleOriginLookup : ModuleOriginLookup
        }
expressionContextToInPath innermostPathDescription context =
    { declarationTypes = context.declarationTypes
    , locallyIntroducedVariableExpressions = context.locallyIntroducedVariableExpressions
    , path = innermostPathDescription :: context.path
    , moduleOriginLookup = context.moduleOriginLookup
    }


{-| Infer types of
value/[`Elm.Syntax.Expression.Function`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Function) declarations
in a module.
-}
expressionDeclarations :
    { importedTypes :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            ModuleTypes
    , otherModuleDeclaredTypes : ModuleTypes
    , moduleOriginLookup : ModuleOriginLookup
    }
    -> List Elm.Syntax.Expression.Function
    ->
        Result
            String
            { arguments : List (TypedNode Pattern)
            , result : TypedNode Expression
            }
expressionDeclarations typesAndOriginLookup syntaxDeclarationExpression =
    Debug.todo ""


type alias ModuleLevelDeclarationTypesInAvailableInModule =
    FastDict.Dict
        -- `[]` means declared in the same module
        Elm.Syntax.ModuleName.ModuleName
        ModuleTypes


{-| Infer types of a
value/[`Elm.Syntax.Expression.Function`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Function) declaration.

Usually you'll have more than one such declaration,
in which case you can use [`expressionDeclarations`](#expressionDeclarations)
-}
expressionDeclaration :
    { importedTypes :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            ModuleTypes
    , otherModuleDeclaredTypes : ModuleTypes
    , moduleOriginLookup : ModuleOriginLookup
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { arguments : List (TypedNode Pattern)
            , result : TypedNode Expression
            }
expressionDeclaration typesAndOriginLookup syntaxDeclarationExpression =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationExpression.declaration |> Elm.Syntax.Node.value
    in
    implementation.arguments
        |> parameterPatternsTypeInfer
            { declarationTypes =
                typesAndOriginLookup.importedTypes
                    |> FastDict.insert [] typesAndOriginLookup.otherModuleDeclaredTypes
            , path = []
            , moduleOriginLookup = typesAndOriginLookup.moduleOriginLookup
            }
        |> Result.andThen
            (\arguments ->
                let
                    declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
                    declarationTypes =
                        typesAndOriginLookup.importedTypes
                            |> FastDict.insert
                                []
                                (typesAndOriginLookup.otherModuleDeclaredTypes
                                    |> (\r ->
                                            { r
                                                | signatures =
                                                    r.signatures
                                                        |> FastDict.insert name
                                                            (type_
                                                                |> typeVariablesMap
                                                                    (\( _, variableName ) -> variableName)
                                                            )
                                            }
                                       )
                                )

                    name : String
                    name =
                        implementation.name |> Elm.Syntax.Node.value

                    resultTypeVariable : Type TypeVariableFromContext
                    resultTypeVariable =
                        TypeVariable ( [], name )

                    type_ : Type TypeVariableFromContext
                    type_ =
                        arguments.nodesReverse
                            |> List.foldl
                                (\argumentTypedNode soFar ->
                                    TypeNotVariable
                                        (TypeFunction
                                            { input = argumentTypedNode.type_
                                            , output = soFar
                                            }
                                        )
                                )
                                resultTypeVariable

                    locallyIntroducedVariableExpressions : FastDict.Dict String (Type TypeVariableFromContext)
                    locallyIntroducedVariableExpressions =
                        arguments.introducedExpressionVariables
                in
                implementation.expression
                    |> expressionTypeInfer
                        { declarationTypes = declarationTypes
                        , locallyIntroducedVariableExpressions = locallyIntroducedVariableExpressions
                        , path = [ "implementation" ]
                        , moduleOriginLookup = typesAndOriginLookup.moduleOriginLookup
                        }
                    |> Result.andThen
                        (\resultInferred ->
                            let
                                resultTypeSubstituted :
                                    Result
                                        String
                                        { node : TypedNode Expression
                                        , equivalentVariables : List (FastSet.Set TypeVariableFromContext)
                                        }
                                resultTypeSubstituted =
                                    -- TODO how to proceed with equivalent variables:
                                    -- → unify types for all equivalent variables
                                    --   and then combine their constraints
                                    --   and then replace all except the combined variable in the final ExpressionTypedNode (tree)
                                    --   TODO what do do with remaining equivalent variables?
                                    resultInferred.node
                                        |> expressionTypedNodeSubstituteVariablesByNotVariables
                                            declarationTypes
                                            resultInferred.substitutions
                            in
                            case syntaxDeclarationExpression.signature of
                                Nothing ->
                                    -- TODO unify+substitute in all places necessary
                                    -- TODO query the original introduced type variable
                                    --      for the final type
                                    Debug.todo ""

                                Just signature ->
                                    Debug.todo
                                        """unify arguments and result
                                         through creating a function with argument types and result type"""
                        )
            )


expressionTypedNodeSubstituteVariablesByNotVariables :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> VariableSubstitutions
    -> TypedNode Expression
    ->
        Result
            String
            { equivalentVariables : List (FastSet.Set TypeVariableFromContext)
            , node : TypedNode Expression
            }
expressionTypedNodeSubstituteVariablesByNotVariables declarationTypes variableSubstitutions expression =
    case variableSubstitutions.variableToType |> FastDict.popMin of
        Nothing ->
            Ok
                { equivalentVariables = variableSubstitutions.equivalentVariables
                , node = expression
                }

        Just ( ( replacementVariable, replacementTypeNotVariable ), remainingReplacements ) ->
            case
                expression
                    |> expressionTypedNodeSubstituteVariableByNotVariable
                        declarationTypes
                        { variable = replacementVariable
                        , type_ = replacementTypeNotVariable
                        }
            of
                Err error ->
                    Err error

                Ok substituted ->
                    case
                        variableSubstitutionsMerge
                            declarationTypes
                            substituted.substitutions
                            { equivalentVariables = variableSubstitutions.equivalentVariables
                            , variableToType = remainingReplacements
                            }
                    of
                        Err error ->
                            Err error

                        Ok substitutionsAfterSubstitution ->
                            expressionTypedNodeSubstituteVariablesByNotVariables
                                declarationTypes
                                substitutionsAfterSubstitution
                                substituted.node


expressionTypedNodeSubstituteVariableByNotVariable :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> { variable : TypeVariableFromContext, type_ : TypeNotVariable TypeVariableFromContext }
    -> TypedNode Expression
    ->
        Result
            String
            { substitutions : VariableSubstitutions
            , node : TypedNode Expression
            }
expressionTypedNodeSubstituteVariableByNotVariable declarationTypes replacement expression =
    -- IGNORE TCO
    case expression.value of
        ExpressionUnit ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = expression
                }

        ExpressionFloat _ ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = expression
                }

        ExpressionChar _ ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = expression
                }

        ExpressionString _ ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = expression
                }

        ExpressionNumber _ ->
            expression.type_
                |> typeSubstituteVariableByNotVariable declarationTypes
                    replacement
                |> Result.map
                    (\substituted ->
                        { substitutions = substituted.substitutions
                        , node =
                            { range = expression.range
                            , value = expression.value
                            , type_ = substituted.type_
                            }
                        }
                    )

        ExpressionReference _ ->
            expression.type_
                |> typeSubstituteVariableByNotVariable declarationTypes
                    replacement
                |> Result.map
                    (\substituted ->
                        { substitutions = substituted.substitutions
                        , node =
                            { range = expression.range
                            , value = expression.value
                            , type_ = substituted.type_
                            }
                        }
                    )

        ExpressionOperatorFunction _ ->
            expression.type_
                |> typeSubstituteVariableByNotVariable declarationTypes
                    replacement
                |> Result.map
                    (\substituted ->
                        { substitutions = substituted.substitutions
                        , node =
                            { range = expression.range
                            , value = expression.value
                            , type_ = substituted.type_
                            }
                        }
                    )

        ExpressionNegation _ ->
            expression.type_
                |> typeSubstituteVariableByNotVariable declarationTypes
                    replacement
                |> Result.map
                    (\substituted ->
                        { substitutions = substituted.substitutions
                        , node =
                            { range = expression.range
                            , value = expression.value
                            , type_ = substituted.type_
                            }
                        }
                    )

        ExpressionParenthesized _ ->
            expression.type_
                |> typeSubstituteVariableByNotVariable declarationTypes
                    replacement
                |> Result.map
                    (\substituted ->
                        { substitutions = substituted.substitutions
                        , node =
                            { range = expression.range
                            , value = expression.value
                            , type_ = substituted.type_
                            }
                        }
                    )

        ExpressionRecordAccess _ ->
            expression.type_
                |> typeSubstituteVariableByNotVariable declarationTypes
                    replacement
                |> Result.map
                    (\substituted ->
                        { substitutions = substituted.substitutions
                        , node =
                            { range = expression.range
                            , value = expression.value
                            , type_ = substituted.type_
                            }
                        }
                    )

        ExpressionRecordAccessFunction _ ->
            expression.type_
                |> typeSubstituteVariableByNotVariable declarationTypes
                    replacement
                |> Result.map
                    (\substituted ->
                        { substitutions = substituted.substitutions
                        , node =
                            { range = expression.range
                            , value = expression.value
                            , type_ = substituted.type_
                            }
                        }
                    )

        ExpressionInfixOperation expressionInfixOperation ->
            Result.map3
                (\leftSubstituted rightSubstituted fullTypeSubstituted ->
                    variableSubstitutionsMerge3 declarationTypes
                        leftSubstituted.substitutions
                        rightSubstituted.substitutions
                        fullTypeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionInfixOperation
                                            { symbol = expressionInfixOperation.symbol
                                            , left = leftSubstituted.node
                                            , right = rightSubstituted.node
                                            }
                                    , type_ = fullTypeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionInfixOperation.left
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionInfixOperation.right
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        ExpressionTuple expressionTuple ->
            Result.map3
                (\part0Substituted part1Substituted fullTypeSubstituted ->
                    variableSubstitutionsMerge3 declarationTypes
                        part0Substituted.substitutions
                        part1Substituted.substitutions
                        fullTypeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionTuple
                                            { part0 = part0Substituted.node
                                            , part1 = part1Substituted.node
                                            }
                                    , type_ = fullTypeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionTuple.part0
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionTuple.part1
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        ExpressionTriple expressionTriple ->
            Result.map4
                (\part0Substituted part1Substituted part2Substituted fullTypeSubstituted ->
                    variableSubstitutionsMerge4 declarationTypes
                        part0Substituted.substitutions
                        part1Substituted.substitutions
                        part2Substituted.substitutions
                        fullTypeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionTuple
                                            { part0 = part0Substituted.node
                                            , part1 = part1Substituted.node
                                            }
                                    , type_ = fullTypeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionTriple.part0
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionTriple.part1
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionTriple.part2
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        ExpressionIfThenElse expressionIfThenElse ->
            Result.map4
                (\conditionSubstituted onTrueSubstituted onFalseSubstituted fullTypeSubstituted ->
                    variableSubstitutionsMerge4 declarationTypes
                        conditionSubstituted.substitutions
                        onTrueSubstituted.substitutions
                        onFalseSubstituted.substitutions
                        fullTypeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionTuple
                                            { part0 = conditionSubstituted.node
                                            , part1 = onTrueSubstituted.node
                                            }
                                    , type_ = fullTypeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionIfThenElse.condition
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionIfThenElse.onTrue
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionIfThenElse.onFalse
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        ExpressionList expressionListElements ->
            Result.map2
                (\elementsSubstituted fullTypeSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        elementsSubstituted.substitutions
                        fullTypeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionList
                                            (elementsSubstituted.nodesReverse
                                                |> List.reverse
                                            )
                                    , type_ = fullTypeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionListElements
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\elementNode soFar ->
                            elementNode
                                |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\elementSubstituted ->
                                        variableSubstitutionsMerge
                                            declarationTypes
                                            elementSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\fullSubstitutions ->
                                                    { substitutions =
                                                        fullSubstitutions
                                                    , nodesReverse =
                                                        elementSubstituted.node
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        ExpressionCall expressionCall ->
            Result.map4
                (\calledSubstituted argument0Substituted argument1UpSubstituted fullTypeSubstituted ->
                    variableSubstitutionsMerge4 declarationTypes
                        calledSubstituted.substitutions
                        argument0Substituted.substitutions
                        argument1UpSubstituted.substitutions
                        fullTypeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionCall
                                            { called = calledSubstituted.node
                                            , argument0 = argument0Substituted.node
                                            , argument1Up =
                                                argument1UpSubstituted.nodesReverse
                                                    |> List.reverse
                                            }
                                    , type_ = fullTypeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionCall.called
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionCall.argument0
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionCall.argument1Up
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\argumentNode soFar ->
                            argumentNode
                                |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\elementSubstituted ->
                                        variableSubstitutionsMerge
                                            declarationTypes
                                            elementSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\fullSubstitutions ->
                                                    { substitutions = fullSubstitutions
                                                    , nodesReverse =
                                                        elementSubstituted.node
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        ExpressionRecord expressionRecordFields ->
            Result.map2
                (\fieldsSubstituted fullTypeSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        fieldsSubstituted.substitutions
                        fullTypeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionRecord
                                            (fieldsSubstituted.nodesReverse
                                                |> List.reverse
                                            )
                                    , type_ = fullTypeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionRecordFields
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\fieldNode soFar ->
                            fieldNode.value
                                |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\fieldValueSubstituted ->
                                        variableSubstitutionsMerge
                                            declarationTypes
                                            fieldValueSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\fullSubstitutions ->
                                                    { substitutions = fullSubstitutions
                                                    , nodesReverse =
                                                        { fieldRange = fieldNode.fieldRange
                                                        , name = fieldNode.name
                                                        , nameRange = fieldNode.nameRange
                                                        , value = fieldValueSubstituted.node
                                                        }
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        ExpressionRecordUpdate expressionRecordUpdate ->
            Result.map2
                (\fieldsSubstituted fullTypeSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        fieldsSubstituted.substitutions
                        fullTypeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionRecordUpdate
                                            { recordVariable =
                                                { value = expressionRecordUpdate.recordVariable.value
                                                , range = expressionRecordUpdate.recordVariable.range
                                                , type_ = fullTypeSubstituted.type_
                                                }
                                            , fields =
                                                fieldsSubstituted.nodesReverse
                                                    |> List.reverse
                                            }
                                    , type_ = fullTypeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionRecordUpdate.fields
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\fieldNode soFar ->
                            fieldNode.value
                                |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\fieldValueSubstituted ->
                                        variableSubstitutionsMerge
                                            declarationTypes
                                            fieldValueSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\fullSubstitutions ->
                                                    { substitutions = fullSubstitutions
                                                    , nodesReverse =
                                                        { fieldRange = fieldNode.fieldRange
                                                        , name = fieldNode.name
                                                        , nameRange = fieldNode.nameRange
                                                        , value = fieldValueSubstituted.node
                                                        }
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                |> Result.andThen identity

        ExpressionLetIn _ ->
            Debug.todo ""

        ExpressionCaseOf _ ->
            Debug.todo ""

        ExpressionLambda _ ->
            Debug.todo ""


equivalentVariablesCreateUnifiedVariable : FastSet.Set TypeVariableFromContext -> Result String TypeVariableFromContext
equivalentVariablesCreateUnifiedVariable set =
    case set |> FastSet.toList of
        [] ->
            Err "implementation bug: equivalent variables set is empty"

        ( headVariableContext, headVariableName ) :: tailVariables ->
            tailVariables
                |> listFoldlWhileOkFrom
                    (headVariableName |> typeVariableConstraint)
                    (\variable soFar ->
                        maybeTypeVariableConstraintMerge
                            (variable |> typeContextVariableName |> typeVariableConstraint)
                            soFar
                    )
                |> Result.map
                    (\unifiedConstraint ->
                        ( headVariableContext
                        , headVariableName
                            |> typeVariableNameReplaceMaybeConstraint unifiedConstraint
                        )
                    )


typeVariableConstraintToString : TypeVariableConstraint -> String
typeVariableConstraintToString constraint =
    case constraint of
        TypeVariableConstraintNumber ->
            "number"

        TypeVariableConstraintAppendable ->
            "appendable"

        TypeVariableConstraintComparable ->
            "comparable"

        TypeVariableConstraintCompappend ->
            "compappend"


typeVariableNameReplaceMaybeConstraint : Maybe TypeVariableConstraint -> String -> String
typeVariableNameReplaceMaybeConstraint maybeConstraint typeVariableNameWithPotentialConstraint =
    let
        typeVariableNameStripConstraint : String
        typeVariableNameStripConstraint =
            case typeVariableNameWithPotentialConstraint |> typeVariableConstraint of
                Nothing ->
                    typeVariableNameWithPotentialConstraint

                Just constraint ->
                    String.dropLeft
                        (constraint |> typeVariableConstraintToString |> String.length)
                        typeVariableNameWithPotentialConstraint
                        |> stringFirstCharToLower
    in
    case maybeConstraint of
        Nothing ->
            typeVariableNameStripConstraint

        Just constraint ->
            (constraint |> typeVariableConstraintToString)
                ++ (typeVariableNameStripConstraint |> stringFirstCharToUpper)


fastDictFoldlWhileOkFrom : ok -> (key -> value -> ok -> Result err ok) -> FastDict.Dict key value -> Result err ok
fastDictFoldlWhileOkFrom initialFolded reduceToResult fastDict =
    fastDict
        |> FastDict.stoppableFoldl
            (\key value soFarOrError ->
                case soFarOrError of
                    Err error ->
                        Err error |> FastDict.Stop

                    Ok soFar ->
                        case reduceToResult key value soFar of
                            Err error ->
                                Err error |> FastDict.Stop

                            Ok foldedWithEntry ->
                                Ok foldedWithEntry |> FastDict.Continue
            )
            (Ok initialFolded)


parameterPatternsTypeInfer :
    { path : List String
    , declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
    , moduleOriginLookup : ModuleOriginLookup
    }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    ->
        Result
            String
            { substitutions : VariableSubstitutions
            , introducedExpressionVariables :
                FastDict.Dict String (Type TypeVariableFromContext)
            , nodesReverse : List (TypedNode Pattern)
            }
parameterPatternsTypeInfer context parameterPatterns =
    parameterPatterns
        |> listFoldlWhileOkFrom
            { substitutions = variableSubstitutionsNone
            , introducedExpressionVariables = FastDict.empty
            , nodesReverse = []
            }
            (\pattern soFar ->
                pattern
                    |> patternTypeInfer context
                    |> Result.andThen
                        (\patternInferred ->
                            variableSubstitutionsMerge context.declarationTypes
                                soFar.substitutions
                                patternInferred.substitutions
                                |> Result.map
                                    (\substitutionsWithPattern ->
                                        { substitutions = substitutionsWithPattern
                                        , nodesReverse =
                                            patternInferred.node
                                                :: soFar.nodesReverse
                                        , introducedExpressionVariables =
                                            FastDict.union
                                                patternInferred.introducedExpressionVariables
                                                soFar.introducedExpressionVariables
                                        }
                                    )
                        )
            )


{-| Extract all known types
from module [metadata](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/),
usually from parsing the `docs.json` of a dependency.

For elm-syntax modules, use [`moduleDeclarationsToTypes`](#moduleDeclarationsToTypes)

-}
moduleInterfaceToTypes :
    Elm.Docs.Module
    -> Result String ModuleTypes
moduleInterfaceToTypes moduleInterface =
    Result.map3
        (\typeAliases choiceTypes signatures ->
            { typeAliases = typeAliases
            , choiceTypes = choiceTypes
            , signatures = signatures
            }
        )
        (moduleInterface.aliases
            |> listFoldlWhileOkFrom FastDict.empty
                (\typeAliasDeclarationInterface soFar ->
                    typeAliasDeclarationInterface.tipe
                        |> interfaceToType
                        |> Result.map
                            (\type_ ->
                                soFar
                                    |> FastDict.insert
                                        typeAliasDeclarationInterface.name
                                        { type_ = type_
                                        , parameters = typeAliasDeclarationInterface.args
                                        }
                            )
                )
        )
        (moduleInterface.unions
            |> listFoldlWhileOkFrom FastDict.empty
                (\declarationChoiceType soFar ->
                    declarationChoiceType.tags
                        |> listFoldlWhileOkFrom
                            FastDict.empty
                            (\( variantName, variantValueInterfaces ) variantsSoFar ->
                                variantValueInterfaces
                                    |> listMapAndCombineOk
                                        (\variantValue ->
                                            variantValue |> interfaceToType
                                        )
                                    |> Result.map
                                        (\variantValues ->
                                            variantsSoFar
                                                |> FastDict.insert
                                                    variantName
                                                    variantValues
                                        )
                            )
                        |> Result.map
                            (\variants ->
                                soFar
                                    |> FastDict.insert
                                        declarationChoiceType.name
                                        { parameters =
                                            declarationChoiceType.args
                                        , variants = variants
                                        }
                            )
                )
        )
        (moduleInterface.values
            |> listFoldlWhileOkFrom FastDict.empty
                (\valueOrFunctionDeclarationInterface soFar ->
                    valueOrFunctionDeclarationInterface.tipe
                        |> interfaceToType
                        |> Result.map
                            (\type_ ->
                                soFar
                                    |> FastDict.insert
                                        valueOrFunctionDeclarationInterface.name
                                        type_
                            )
                )
        )


interfaceToType : Elm.Type.Type -> Result String (Type String)
interfaceToType typeInterface =
    -- IGNORE TCO
    case typeInterface of
        Elm.Type.Var name ->
            Ok (TypeVariable name)

        Elm.Type.Lambda functionInput functionOutput ->
            Result.map2
                (\input output ->
                    TypeNotVariable
                        (TypeFunction { input = input, output = output })
                )
                (functionInput |> interfaceToType)
                (functionOutput |> interfaceToType)

        Elm.Type.Tuple parts ->
            case parts of
                [] ->
                    Ok (TypeNotVariable TypeUnit)

                [ inParens ] ->
                    inParens |> interfaceToType

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            TypeNotVariable
                                (TypeTuple { part0 = part0, part1 = part1 })
                        )
                        (tuplePart1 |> interfaceToType)
                        (tuplePart0 |> interfaceToType)

                [ triplePart0, triplePart1, triplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            TypeNotVariable
                                (TypeTriple { part0 = part0, part1 = part1, part2 = part2 })
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
                            TypeNotVariable
                                (TypeConstruct
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
                (\fields -> TypeNotVariable (TypeRecord fields))
                (fieldInterfaces
                    |> listFoldlWhileOkFrom FastDict.empty
                        (\( name, valueInterface ) soFar ->
                            valueInterface
                                |> interfaceToType
                                |> Result.map
                                    (\value ->
                                        soFar |> FastDict.insert name value
                                    )
                        )
                )

        Elm.Type.Record fieldInterfaces (Just extendedRecordVariable) ->
            Result.map
                (\fields ->
                    TypeNotVariable
                        (TypeRecordExtension
                            { fields = fields
                            , recordVariable = extendedRecordVariable
                            }
                        )
                )
                (fieldInterfaces
                    |> listFoldlWhileOkFrom FastDict.empty
                        (\( name, valueInterface ) soFar ->
                            valueInterface
                                |> interfaceToType
                                |> Result.map
                                    (\value ->
                                        soFar |> FastDict.insert name value
                                    )
                        )
                )


{-| Extract all known types
from declarations within a module.
No effort is being made to infer types at this stage.

For dependency modules, use [`moduleInterfaceToTypes`](#moduleInterfaceToTypes)

-}
moduleDeclarationsToTypes :
    ModuleOriginLookup
    -> List Elm.Syntax.Declaration.Declaration
    -> Result String ModuleTypes
moduleDeclarationsToTypes moduleOriginLookup declarations =
    declarations
        |> listFoldlWhileOkFrom
            { signatures = FastDict.empty
            , typeAliases = FastDict.empty
            , choiceTypes = FastDict.empty
            }
            (\declaration soFar ->
                case declaration of
                    Elm.Syntax.Declaration.InfixDeclaration _ ->
                        Ok soFar

                    Elm.Syntax.Declaration.Destructuring _ _ ->
                        Err "destructuring at the module level is invalid syntax"

                    Elm.Syntax.Declaration.FunctionDeclaration declarationValueOrFunction ->
                        case declarationValueOrFunction.signature of
                            Nothing ->
                                Ok soFar

                            Just (Elm.Syntax.Node.Node _ declarationValueOrFunctionSignature) ->
                                declarationValueOrFunctionSignature.typeAnnotation
                                    |> Elm.Syntax.Node.value
                                    |> syntaxToType moduleOriginLookup
                                    |> Result.map
                                        (\type_ ->
                                            { signatures =
                                                soFar.signatures
                                                    |> FastDict.insert
                                                        (declarationValueOrFunctionSignature.name |> Elm.Syntax.Node.value)
                                                        type_
                                            , typeAliases = soFar.typeAliases
                                            , choiceTypes = soFar.choiceTypes
                                            }
                                        )

                    Elm.Syntax.Declaration.AliasDeclaration declarationTypeAlias ->
                        declarationTypeAlias.typeAnnotation
                            |> Elm.Syntax.Node.value
                            |> syntaxToType moduleOriginLookup
                            |> Result.map
                                (\type_ ->
                                    { signatures = soFar.signatures
                                    , typeAliases =
                                        soFar.typeAliases
                                            |> FastDict.insert
                                                (declarationTypeAlias.name |> Elm.Syntax.Node.value)
                                                { parameters =
                                                    declarationTypeAlias.generics
                                                        |> List.map Elm.Syntax.Node.value
                                                , type_ = type_
                                                }
                                    , choiceTypes = soFar.choiceTypes
                                    }
                                )

                    Elm.Syntax.Declaration.CustomTypeDeclaration declarationChoiceType ->
                        declarationChoiceType.constructors
                            |> listFoldlWhileOkFrom
                                FastDict.empty
                                (\(Elm.Syntax.Node.Node _ variant) variantsSoFar ->
                                    variant.arguments
                                        |> listMapAndCombineOk
                                            (\(Elm.Syntax.Node.Node _ variantValue) ->
                                                variantValue |> syntaxToType moduleOriginLookup
                                            )
                                        |> Result.map
                                            (\variantValues ->
                                                variantsSoFar
                                                    |> FastDict.insert
                                                        (variant.name |> Elm.Syntax.Node.value)
                                                        variantValues
                                            )
                                )
                            |> Result.map
                                (\variants ->
                                    { signatures = soFar.signatures
                                    , typeAliases = soFar.typeAliases
                                    , choiceTypes =
                                        soFar.choiceTypes
                                            |> FastDict.insert
                                                (declarationChoiceType.name |> Elm.Syntax.Node.value)
                                                { parameters =
                                                    declarationChoiceType.generics
                                                        |> List.map Elm.Syntax.Node.value
                                                , variants = variants
                                                }
                                    }
                                )

                    Elm.Syntax.Declaration.PortDeclaration declarationPortSignature ->
                        declarationPortSignature.typeAnnotation
                            |> Elm.Syntax.Node.value
                            |> syntaxToType moduleOriginLookup
                            |> Result.map
                                (\type_ ->
                                    { signatures =
                                        soFar.signatures
                                            |> FastDict.insert
                                                (declarationPortSignature.name |> Elm.Syntax.Node.value)
                                                type_
                                    , typeAliases = soFar.typeAliases
                                    , choiceTypes = soFar.choiceTypes
                                    }
                                )
            )


listFoldlWhileOkFrom :
    okFolded
    -> (a -> okFolded -> Result err okFolded)
    -> List a
    -> Result err okFolded
listFoldlWhileOkFrom initialOkFolded reduceOnOk list =
    case list of
        [] ->
            Ok initialOkFolded

        head :: tail ->
            case initialOkFolded |> reduceOnOk head of
                Err error ->
                    Err error

                Ok okFoldedWithHead ->
                    listFoldlWhileOkFrom okFoldedWithHead reduceOnOk tail


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


elmCoreTypesGeneratedFromDocsJson :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        ModuleTypes
elmCoreTypesGeneratedFromDocsJson =
    FastDict.fromList
        [ ( [ "Array" ]
          , { signatures =
                FastDict.fromList
                    [ ( "append"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Array" ]
                                            , name = "Array"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "empty"
                      , TypeNotVariable
                            (TypeConstruct
                                { moduleOrigin = [ "Array" ]
                                , name = "Array"
                                , arguments =
                                    [ TypeVariable "a" ]
                                }
                            )
                      )
                    , ( "filter"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldl"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Array"
                                                                        ]
                                                                    , name =
                                                                        "Array"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldr"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Array"
                                                                        ]
                                                                    , name =
                                                                        "Array"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Array" ]
                                            , name = "Array"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "get"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "indexedMap"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "a"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "initialize"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Int"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "a"
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isEmpty"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Array" ]
                                            , name = "Array"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "length"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Array" ]
                                            , name = "Array"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "b"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "push"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "repeat"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "set"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Array"
                                                                        ]
                                                                    , name =
                                                                        "Array"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Array"
                                                                        ]
                                                                    , name =
                                                                        "Array"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "slice"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Array"
                                                                        ]
                                                                    , name =
                                                                        "Array"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Array"
                                                                        ]
                                                                    , name =
                                                                        "Array"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toIndexedList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Array" ]
                                            , name = "Array"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "a"
                                                        , part1 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Int"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Array" ]
                                            , name = "Array"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Array"
                      , { parameters = [ "a" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    ]
            }
          )
        , ( [ "Basics" ]
          , { signatures =
                FastDict.fromList
                    [ ( "abs"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable "number"
                                , output =
                                    TypeVariable "number"
                                }
                            )
                      )
                    , ( "acos"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "always"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeVariable
                                                    "a"
                                            }
                                        )
                                }
                            )
                      )
                    , ( "asin"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "atan"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "atan2"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "ceiling"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "clamp"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable "number"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "number"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "number"
                                                        , output =
                                                            TypeVariable
                                                                "number"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "compare"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Order"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "cos"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "degrees"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "e"
                      , TypeNotVariable
                            (TypeConstruct
                                { moduleOrigin = [ "Basics" ]
                                , name = "Float"
                                , arguments = []
                                }
                            )
                      )
                    , ( "floor"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromPolar"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeTuple
                                            { part0 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            , part1 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeTuple
                                            { part0 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            , part1 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "identity"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output = TypeVariable "a"
                                }
                            )
                      )
                    , ( "isInfinite"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isNaN"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "logBase"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "max"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeVariable
                                                    "comparable"
                                            }
                                        )
                                }
                            )
                      )
                    , ( "min"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeVariable
                                                    "comparable"
                                            }
                                        )
                                }
                            )
                      )
                    , ( "modBy"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "negate"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable "number"
                                , output =
                                    TypeVariable "number"
                                }
                            )
                      )
                    , ( "never"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Never"
                                            , arguments = []
                                            }
                                        )
                                , output = TypeVariable "a"
                                }
                            )
                      )
                    , ( "not"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "pi"
                      , TypeNotVariable
                            (TypeConstruct
                                { moduleOrigin = [ "Basics" ]
                                , name = "Float"
                                , arguments = []
                                }
                            )
                      )
                    , ( "radians"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "remainderBy"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "round"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sin"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sqrt"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "tan"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toFloat"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toPolar"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeTuple
                                            { part0 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            , part1 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeTuple
                                            { part0 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            , part1 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "truncate"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "turns"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "xor"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Bool"
                      , { parameters = []
                        , variants =
                            FastDict.fromList
                                [ ( "True", [] ), ( "False", [] ) ]
                        }
                      )
                    , ( "Float"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    , ( "Int"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    , ( "Never"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    , ( "Order"
                      , { parameters = []
                        , variants =
                            FastDict.fromList
                                [ ( "LT", [] ), ( "EQ", [] ), ( "GT", [] ) ]
                        }
                      )
                    ]
            }
          )
        , ( [ "Bitwise" ]
          , { signatures =
                FastDict.fromList
                    [ ( "and"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "complement"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "or"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "shiftLeftBy"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "shiftRightBy"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "shiftRightZfBy"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "xor"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes = FastDict.fromList []
            }
          )
        , ( [ "Char" ]
          , { signatures =
                FastDict.fromList
                    [ ( "fromCode"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isAlpha"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isAlphaNum"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isDigit"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isHexDigit"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isLower"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isOctDigit"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isUpper"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toCode"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toLocaleLower"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toLocaleUpper"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toLower"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toUpper"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Char"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    ]
            }
          )
        , ( [ "Debug" ]
          , { signatures =
                FastDict.fromList
                    [ ( "log"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "a"
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toString"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "todo"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output = TypeVariable "a"
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes = FastDict.fromList []
            }
          )
        , ( [ "Dict" ]
          , { signatures =
                FastDict.fromList
                    [ ( "diff"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                , TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "empty"
                      , TypeNotVariable
                            (TypeConstruct
                                { moduleOrigin = [ "Dict" ]
                                , name = "Dict"
                                , arguments =
                                    [ TypeVariable "k"
                                    , TypeVariable "v"
                                    ]
                                }
                            )
                      )
                    , ( "filter"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "v"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Bool"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldl"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "k"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "v"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "b"
                                                                    , output =
                                                                        TypeVariable
                                                                            "b"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "k"
                                                                        , TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldr"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "k"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "v"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "b"
                                                                    , output =
                                                                        TypeVariable
                                                                            "b"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "k"
                                                                        , TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "v"
                                                        , part1 =
                                                            TypeVariable
                                                                "comparable"
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                , TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "get"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "insert"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "v"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "comparable"
                                                                        , TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "comparable"
                                                                        , TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "intersect"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                , TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isEmpty"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "k"
                                                , TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "keys"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "k"
                                                , TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "k"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "k"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "a"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "k"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "k"
                                                            , TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "member"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "merge"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "a"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "result"
                                                                    , output =
                                                                        TypeVariable
                                                                            "result"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "comparable"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "a"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "b"
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeVariable
                                                                                                    "result"
                                                                                            , output =
                                                                                                TypeVariable
                                                                                                    "result"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "comparable"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "b"
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeVariable
                                                                                                    "result"
                                                                                            , output =
                                                                                                TypeVariable
                                                                                                    "result"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Dict"
                                                                                    ]
                                                                                , name =
                                                                                    "Dict"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "comparable"
                                                                                    , TypeVariable
                                                                                        "a"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Dict"
                                                                                                ]
                                                                                            , name =
                                                                                                "Dict"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "comparable"
                                                                                                , TypeVariable
                                                                                                    "b"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeVariable
                                                                                                    "result"
                                                                                            , output =
                                                                                                TypeVariable
                                                                                                    "result"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "partition"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "v"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Bool"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "comparable"
                                                                        , TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        , part1 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "comparable"
                                                                        , TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "remove"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "singleton"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "v"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "size"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "k"
                                                , TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "k"
                                                , TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "v"
                                                        , part1 =
                                                            TypeVariable
                                                                "k"
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "union"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                , TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            , TypeVariable
                                                                "v"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "update"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Maybe"
                                                                        ]
                                                                    , name =
                                                                        "Maybe"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Maybe"
                                                                        ]
                                                                    , name =
                                                                        "Maybe"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "comparable"
                                                                        , TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "comparable"
                                                                        , TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "values"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Dict" ]
                                            , name = "Dict"
                                            , arguments =
                                                [ TypeVariable
                                                    "k"
                                                , TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "v"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Dict"
                      , { parameters = [ "k", "v" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    ]
            }
          )
        , ( [ "List" ]
          , { signatures =
                FastDict.fromList
                    [ ( "all"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "any"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "append"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "concat"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "concatMap"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "drop"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "filter"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "filterMap"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldl"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldr"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "head"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "indexedMap"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "a"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "intersperse"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isEmpty"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "length"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "b"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map2"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "result"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "result"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map3"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeVariable
                                                                            "result"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "List"
                                                                                    ]
                                                                                , name =
                                                                                    "List"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "List"
                                                                                    ]
                                                                                , name =
                                                                                    "List"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "result"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map4"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    TypeVariable
                                                                                        "result"
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "List"
                                                                                    ]
                                                                                , name =
                                                                                    "List"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "List"
                                                                                                ]
                                                                                            , name =
                                                                                                "List"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "List"
                                                                                                ]
                                                                                            , name =
                                                                                                "List"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "result"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map5"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                TypeVariable
                                                                                                    "result"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "List"
                                                                                    ]
                                                                                , name =
                                                                                    "List"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "List"
                                                                                                ]
                                                                                            , name =
                                                                                                "List"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeNotVariable
                                                                                                    (TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "List"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "List"
                                                                                                        , arguments =
                                                                                                            [ TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                TypeNotVariable
                                                                                                    (TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "List"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "List"
                                                                                                        , arguments =
                                                                                                            [ TypeVariable
                                                                                                                "result"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "maximum"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "member"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "minimum"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "partition"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , part1 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "product"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "number"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeVariable "number"
                                }
                            )
                      )
                    , ( "range"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Int"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "repeat"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "reverse"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "singleton"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sort"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sortBy"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "comparable"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sortWith"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "a"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Order"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sum"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "number"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeVariable "number"
                                }
                            )
                      )
                    , ( "tail"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "take"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "unzip"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "b"
                                                        , part1 =
                                                            TypeVariable
                                                                "a"
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeTuple
                                            { part0 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            , part1 =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.singleton
                    "List"
                    { parameters = [ "a" ], variants = FastDict.empty }
            }
          )
        , ( [ "Maybe" ]
          , { signatures =
                FastDict.fromList
                    [ ( "andThen"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "b"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map2"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "value"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Maybe"
                                                                        ]
                                                                    , name =
                                                                        "Maybe"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Maybe"
                                                                        ]
                                                                    , name =
                                                                        "Maybe"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "value"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map3"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeVariable
                                                                            "value"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Maybe"
                                                                        ]
                                                                    , name =
                                                                        "Maybe"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Maybe"
                                                                                    ]
                                                                                , name =
                                                                                    "Maybe"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Maybe"
                                                                                    ]
                                                                                , name =
                                                                                    "Maybe"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "value"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map4"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    TypeVariable
                                                                                        "value"
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Maybe"
                                                                        ]
                                                                    , name =
                                                                        "Maybe"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Maybe"
                                                                                    ]
                                                                                , name =
                                                                                    "Maybe"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Maybe"
                                                                                                ]
                                                                                            , name =
                                                                                                "Maybe"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Maybe"
                                                                                                ]
                                                                                            , name =
                                                                                                "Maybe"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "value"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map5"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                TypeVariable
                                                                                                    "value"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Maybe"
                                                                        ]
                                                                    , name =
                                                                        "Maybe"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Maybe"
                                                                                    ]
                                                                                , name =
                                                                                    "Maybe"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Maybe"
                                                                                                ]
                                                                                            , name =
                                                                                                "Maybe"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeNotVariable
                                                                                                    (TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Maybe"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Maybe"
                                                                                                        , arguments =
                                                                                                            [ TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                TypeNotVariable
                                                                                                    (TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Maybe"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Maybe"
                                                                                                        , arguments =
                                                                                                            [ TypeVariable
                                                                                                                "value"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "withDefault"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeVariable
                                                    "a"
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Maybe"
                      , { parameters = [ "a" ]
                        , variants =
                            FastDict.fromList
                                [ ( "Just"
                                  , [ TypeVariable "a" ]
                                  )
                                , ( "Nothing", [] )
                                ]
                        }
                      )
                    ]
            }
          )
        , ( [ "Platform" ]
          , { signatures =
                FastDict.fromList
                    [ ( "sendToApp"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Platform" ]
                                            , name = "Router"
                                            , arguments =
                                                [ TypeVariable
                                                    "msg"
                                                , TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "msg"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeNotVariable
                                                                TypeUnit
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sendToSelf"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Platform" ]
                                            , name = "Router"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                , TypeVariable
                                                    "msg"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "msg"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeNotVariable
                                                                TypeUnit
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "worker"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeRecord
                                            (FastDict.fromList
                                                [ ( "init"
                                                  , TypeNotVariable
                                                        (TypeFunction
                                                            { input =
                                                                TypeVariable
                                                                    "flags"
                                                            , output =
                                                                TypeNotVariable
                                                                    (TypeTuple
                                                                        { part0 =
                                                                            TypeNotVariable
                                                                                (TypeConstruct
                                                                                    { moduleOrigin =
                                                                                        [ "Platform"
                                                                                        , "Cmd"
                                                                                        ]
                                                                                    , name =
                                                                                        "Cmd"
                                                                                    , arguments =
                                                                                        [ TypeVariable
                                                                                            "msg"
                                                                                        ]
                                                                                    }
                                                                                )
                                                                        , part1 =
                                                                            TypeVariable
                                                                                "model"
                                                                        }
                                                                    )
                                                            }
                                                        )
                                                  )
                                                , ( "subscriptions"
                                                  , TypeNotVariable
                                                        (TypeFunction
                                                            { input =
                                                                TypeVariable
                                                                    "model"
                                                            , output =
                                                                TypeNotVariable
                                                                    (TypeConstruct
                                                                        { moduleOrigin =
                                                                            [ "Platform"
                                                                            , "Sub"
                                                                            ]
                                                                        , name =
                                                                            "Sub"
                                                                        , arguments =
                                                                            [ TypeVariable
                                                                                "msg"
                                                                            ]
                                                                        }
                                                                    )
                                                            }
                                                        )
                                                  )
                                                , ( "update"
                                                  , TypeNotVariable
                                                        (TypeFunction
                                                            { input =
                                                                TypeVariable
                                                                    "msg"
                                                            , output =
                                                                TypeNotVariable
                                                                    (TypeFunction
                                                                        { input =
                                                                            TypeVariable
                                                                                "model"
                                                                        , output =
                                                                            TypeNotVariable
                                                                                (TypeTuple
                                                                                    { part0 =
                                                                                        TypeNotVariable
                                                                                            (TypeConstruct
                                                                                                { moduleOrigin =
                                                                                                    [ "Platform"
                                                                                                    , "Cmd"
                                                                                                    ]
                                                                                                , name =
                                                                                                    "Cmd"
                                                                                                , arguments =
                                                                                                    [ TypeVariable
                                                                                                        "msg"
                                                                                                    ]
                                                                                                }
                                                                                            )
                                                                                    , part1 =
                                                                                        TypeVariable
                                                                                            "model"
                                                                                    }
                                                                                )
                                                                        }
                                                                    )
                                                            }
                                                        )
                                                  )
                                                ]
                                            )
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Platform" ]
                                            , name = "Program"
                                            , arguments =
                                                [ TypeVariable
                                                    "flags"
                                                , TypeVariable
                                                    "model"
                                                , TypeVariable
                                                    "msg"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "ProcessId"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    , ( "Program"
                      , { parameters = [ "flags", "model", "msg" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    , ( "Router"
                      , { parameters = [ "appMsg", "selfMsg" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    , ( "Task"
                      , { parameters = [ "err", "ok" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    ]
            }
          )
        , ( [ "Platform", "Cmd" ]
          , { signatures =
                FastDict.fromList
                    [ ( "batch"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform"
                                                            , "Cmd"
                                                            ]
                                                        , name = "Cmd"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "msg"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin =
                                                [ "Platform", "Cmd" ]
                                            , name = "Cmd"
                                            , arguments =
                                                [ TypeVariable
                                                    "msg"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "msg"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform"
                                                            , "Cmd"
                                                            ]
                                                        , name = "Cmd"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform"
                                                            , "Cmd"
                                                            ]
                                                        , name = "Cmd"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "msg"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "none"
                      , TypeNotVariable
                            (TypeConstruct
                                { moduleOrigin = [ "Platform", "Cmd" ]
                                , name = "Cmd"
                                , arguments =
                                    [ TypeVariable "msg" ]
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Cmd"
                      , { parameters = [ "msg" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    ]
            }
          )
        , ( [ "Platform", "Sub" ]
          , { signatures =
                FastDict.fromList
                    [ ( "batch"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform"
                                                            , "Sub"
                                                            ]
                                                        , name = "Sub"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "msg"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin =
                                                [ "Platform", "Sub" ]
                                            , name = "Sub"
                                            , arguments =
                                                [ TypeVariable
                                                    "msg"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "msg"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform"
                                                            , "Sub"
                                                            ]
                                                        , name = "Sub"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform"
                                                            , "Sub"
                                                            ]
                                                        , name = "Sub"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "msg"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "none"
                      , TypeNotVariable
                            (TypeConstruct
                                { moduleOrigin = [ "Platform", "Sub" ]
                                , name = "Sub"
                                , arguments =
                                    [ TypeVariable "msg" ]
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Sub"
                      , { parameters = [ "msg" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    ]
            }
          )
        , ( [ "Process" ]
          , { signatures =
                FastDict.fromList
                    [ ( "kill"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Process" ]
                                            , name = "Id"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Task" ]
                                            , name = "Task"
                                            , arguments =
                                                [ TypeVariable
                                                    "x"
                                                , TypeNotVariable
                                                    TypeUnit
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sleep"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Task" ]
                                            , name = "Task"
                                            , arguments =
                                                [ TypeVariable
                                                    "x"
                                                , TypeNotVariable
                                                    TypeUnit
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "spawn"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Task" ]
                                            , name = "Task"
                                            , arguments =
                                                [ TypeVariable
                                                    "x"
                                                , TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Task" ]
                                            , name = "Task"
                                            , arguments =
                                                [ TypeVariable
                                                    "y"
                                                , TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Process" ]
                                                        , name = "Id"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases =
                FastDict.fromList
                    [ ( "Id"
                      , { parameters = []
                        , type_ =
                            TypeNotVariable
                                (TypeConstruct
                                    { moduleOrigin = [ "Platform" ]
                                    , name = "ProcessId"
                                    , arguments = []
                                    }
                                )
                        }
                      )
                    ]
            , choiceTypes = FastDict.fromList []
            }
          )
        , ( [ "Result" ]
          , { signatures =
                FastDict.fromList
                    [ ( "andThen"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromMaybe"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "x"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "value"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "value"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map2"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "value"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Result"
                                                                        ]
                                                                    , name =
                                                                        "Result"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Result"
                                                                        ]
                                                                    , name =
                                                                        "Result"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "value"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map3"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeVariable
                                                                            "value"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Result"
                                                                        ]
                                                                    , name =
                                                                        "Result"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Result"
                                                                                    ]
                                                                                , name =
                                                                                    "Result"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "x"
                                                                                    , TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Result"
                                                                                    ]
                                                                                , name =
                                                                                    "Result"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "x"
                                                                                    , TypeVariable
                                                                                        "value"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map4"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    TypeVariable
                                                                                        "value"
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Result"
                                                                        ]
                                                                    , name =
                                                                        "Result"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Result"
                                                                                    ]
                                                                                , name =
                                                                                    "Result"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "x"
                                                                                    , TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Result"
                                                                                                ]
                                                                                            , name =
                                                                                                "Result"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "x"
                                                                                                , TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Result"
                                                                                                ]
                                                                                            , name =
                                                                                                "Result"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "x"
                                                                                                , TypeVariable
                                                                                                    "value"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map5"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                TypeVariable
                                                                                                    "value"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Result"
                                                                        ]
                                                                    , name =
                                                                        "Result"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Result"
                                                                                    ]
                                                                                , name =
                                                                                    "Result"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "x"
                                                                                    , TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Result"
                                                                                                ]
                                                                                            , name =
                                                                                                "Result"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "x"
                                                                                                , TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeNotVariable
                                                                                                    (TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Result"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Result"
                                                                                                        , arguments =
                                                                                                            [ TypeVariable
                                                                                                                "x"
                                                                                                            , TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                TypeNotVariable
                                                                                                    (TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Result"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Result"
                                                                                                        , arguments =
                                                                                                            [ TypeVariable
                                                                                                                "x"
                                                                                                            , TypeVariable
                                                                                                                "value"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "mapError"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "x"
                                            , output =
                                                TypeVariable
                                                    "y"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "y"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toMaybe"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Result" ]
                                            , name = "Result"
                                            , arguments =
                                                [ TypeVariable
                                                    "x"
                                                , TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "withDefault"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeVariable
                                                    "a"
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Result"
                      , { parameters = [ "error", "value" ]
                        , variants =
                            FastDict.fromList
                                [ ( "Ok"
                                  , [ TypeVariable "value" ]
                                  )
                                , ( "Err"
                                  , [ TypeVariable "error" ]
                                  )
                                ]
                        }
                      )
                    ]
            }
          )
        , ( [ "Set" ]
          , { signatures =
                FastDict.fromList
                    [ ( "diff"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Set" ]
                                            , name = "Set"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "empty"
                      , TypeNotVariable
                            (TypeConstruct
                                { moduleOrigin = [ "Set" ]
                                , name = "Set"
                                , arguments =
                                    [ TypeVariable "a" ]
                                }
                            )
                      )
                    , ( "filter"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldl"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Set"
                                                                        ]
                                                                    , name =
                                                                        "Set"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldr"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Set"
                                                                        ]
                                                                    , name =
                                                                        "Set"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Set" ]
                                            , name = "Set"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "insert"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "intersect"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Set" ]
                                            , name = "Set"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isEmpty"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Set" ]
                                            , name = "Set"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeVariable
                                                    "comparable2"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable2"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "member"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "partition"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "comparable"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Set"
                                                                        ]
                                                                    , name =
                                                                        "Set"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "comparable"
                                                                        ]
                                                                    }
                                                                )
                                                        , part1 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Set"
                                                                        ]
                                                                    , name =
                                                                        "Set"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "comparable"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "remove"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "singleton"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeVariable
                                        "comparable"
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Set" ]
                                            , name = "Set"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "size"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Set" ]
                                            , name = "Set"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Set" ]
                                            , name = "Set"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "union"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Set" ]
                                            , name = "Set"
                                            , arguments =
                                                [ TypeVariable
                                                    "comparable"
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "comparable"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Set"
                      , { parameters = [ "t" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    ]
            }
          )
        , ( [ "String" ]
          , { signatures =
                FastDict.fromList
                    [ ( "all"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "any"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "append"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "concat"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "cons"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "contains"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "dropLeft"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "dropRight"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "endsWith"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "filter"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldl"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "foldr"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            TypeVariable
                                                                "b"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromChar"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Char" ]
                                            , name = "Char"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromFloat"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromInt"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "indexes"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Int"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "indices"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Int"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "isEmpty"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "join"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "left"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "length"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "lines"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "pad"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "padLeft"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "padRight"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "repeat"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "replace"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "reverse"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "right"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "slice"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "split"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "startsWith"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toFloat"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toInt"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
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
                    , ( "toList"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Char" ]
                                                        , name = "Char"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toLower"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "toUpper"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "trim"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "trimLeft"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "trimRight"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "uncons"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , part1 =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Char"
                                                                        ]
                                                                    , name =
                                                                        "Char"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "words"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "String"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    ]
            }
          )
        , ( [ "Task" ]
          , { signatures =
                FastDict.fromList
                    [ ( "andThen"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "attempt"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeVariable
                                                    "msg"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform"
                                                            , "Cmd"
                                                            ]
                                                        , name = "Cmd"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "msg"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fail"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "x"
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Task" ]
                                            , name = "Task"
                                            , arguments =
                                                [ TypeVariable
                                                    "x"
                                                , TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "b"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map2"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "result"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Task"
                                                                        ]
                                                                    , name =
                                                                        "Task"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Task"
                                                                        ]
                                                                    , name =
                                                                        "Task"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "result"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map3"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeVariable
                                                                            "result"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Task"
                                                                        ]
                                                                    , name =
                                                                        "Task"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Task"
                                                                                    ]
                                                                                , name =
                                                                                    "Task"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "x"
                                                                                    , TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Task"
                                                                                    ]
                                                                                , name =
                                                                                    "Task"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "x"
                                                                                    , TypeVariable
                                                                                        "result"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map4"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    TypeVariable
                                                                                        "result"
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Task"
                                                                        ]
                                                                    , name =
                                                                        "Task"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Task"
                                                                                    ]
                                                                                , name =
                                                                                    "Task"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "x"
                                                                                    , TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Task"
                                                                                                ]
                                                                                            , name =
                                                                                                "Task"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "x"
                                                                                                , TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Task"
                                                                                                ]
                                                                                            , name =
                                                                                                "Task"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "x"
                                                                                                , TypeVariable
                                                                                                    "result"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map5"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                TypeVariable
                                                                                                    "result"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Task"
                                                                        ]
                                                                    , name =
                                                                        "Task"
                                                                    , arguments =
                                                                        [ TypeVariable
                                                                            "x"
                                                                        , TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input =
                                                                        TypeNotVariable
                                                                            (TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Task"
                                                                                    ]
                                                                                , name =
                                                                                    "Task"
                                                                                , arguments =
                                                                                    [ TypeVariable
                                                                                        "x"
                                                                                    , TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        TypeNotVariable
                                                                            (TypeFunction
                                                                                { input =
                                                                                    TypeNotVariable
                                                                                        (TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Task"
                                                                                                ]
                                                                                            , name =
                                                                                                "Task"
                                                                                            , arguments =
                                                                                                [ TypeVariable
                                                                                                    "x"
                                                                                                , TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    TypeNotVariable
                                                                                        (TypeFunction
                                                                                            { input =
                                                                                                TypeNotVariable
                                                                                                    (TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Task"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Task"
                                                                                                        , arguments =
                                                                                                            [ TypeVariable
                                                                                                                "x"
                                                                                                            , TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                TypeNotVariable
                                                                                                    (TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Task"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Task"
                                                                                                        , arguments =
                                                                                                            [ TypeVariable
                                                                                                                "x"
                                                                                                            , TypeVariable
                                                                                                                "result"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "mapError"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "x"
                                            , output =
                                                TypeVariable
                                                    "y"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "y"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "onError"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "x"
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "y"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "y"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "perform"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "msg"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeNotVariable
                                                                (TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Basics"
                                                                        ]
                                                                    , name =
                                                                        "Never"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Platform"
                                                            , "Cmd"
                                                            ]
                                                        , name = "Cmd"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "msg"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sequence"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Task" ]
                                                        , name = "Task"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "x"
                                                            , TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Task" ]
                                            , name = "Task"
                                            , arguments =
                                                [ TypeVariable
                                                    "x"
                                                , TypeNotVariable
                                                    (TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "succeed"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeConstruct
                                            { moduleOrigin = [ "Task" ]
                                            , name = "Task"
                                            , arguments =
                                                [ TypeVariable
                                                    "x"
                                                , TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases =
                FastDict.fromList
                    [ ( "Task"
                      , { parameters = [ "x", "a" ]
                        , type_ =
                            TypeNotVariable
                                (TypeConstruct
                                    { moduleOrigin = [ "Platform" ]
                                    , name = "Task"
                                    , arguments =
                                        [ TypeVariable "x"
                                        , TypeVariable "a"
                                        ]
                                    }
                                )
                        }
                      )
                    ]
            , choiceTypes = FastDict.fromList []
            }
          )
        , ( [ "Tuple" ]
          , { signatures =
                FastDict.fromList
                    [ ( "first"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeTuple
                                            { part0 =
                                                TypeVariable
                                                    "b"
                                            , part1 =
                                                TypeVariable
                                                    "a"
                                            }
                                        )
                                , output = TypeVariable "a"
                                }
                            )
                      )
                    , ( "mapBoth"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "x"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeVariable
                                                                "b"
                                                        , output =
                                                            TypeVariable
                                                                "y"
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeFunction
                                                        { input =
                                                            TypeNotVariable
                                                                (TypeTuple
                                                                    { part0 =
                                                                        TypeVariable
                                                                            "b"
                                                                    , part1 =
                                                                        TypeVariable
                                                                            "a"
                                                                    }
                                                                )
                                                        , output =
                                                            TypeNotVariable
                                                                (TypeTuple
                                                                    { part0 =
                                                                        TypeVariable
                                                                            "y"
                                                                    , part1 =
                                                                        TypeVariable
                                                                            "x"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "mapFirst"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "a"
                                            , output =
                                                TypeVariable
                                                    "x"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "b"
                                                        , part1 =
                                                            TypeVariable
                                                                "a"
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "b"
                                                        , part1 =
                                                            TypeVariable
                                                                "x"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "mapSecond"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeVariable
                                                    "y"
                                            }
                                        )
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "b"
                                                        , part1 =
                                                            TypeVariable
                                                                "a"
                                                        }
                                                    )
                                            , output =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "y"
                                                        , part1 =
                                                            TypeVariable
                                                                "a"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "pair"
                      , TypeNotVariable
                            (TypeFunction
                                { input = TypeVariable "a"
                                , output =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input =
                                                TypeVariable
                                                    "b"
                                            , output =
                                                TypeNotVariable
                                                    (TypeTuple
                                                        { part0 =
                                                            TypeVariable
                                                                "b"
                                                        , part1 =
                                                            TypeVariable
                                                                "a"
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "second"
                      , TypeNotVariable
                            (TypeFunction
                                { input =
                                    TypeNotVariable
                                        (TypeTuple
                                            { part0 =
                                                TypeVariable
                                                    "b"
                                            , part1 =
                                                TypeVariable
                                                    "a"
                                            }
                                        )
                                , output = TypeVariable "b"
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes = FastDict.fromList []
            }
          )
        ]
