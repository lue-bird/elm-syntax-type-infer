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

    ( 0, 1 )
    -- ( number, number )
    ( identity, List.map identity )
    -- ( a -> a, List a -> List a )

would be incorrect inferences because
the `number` in `0` and `1` or `a` in `identity` and `List.map` are not related
and can be different types.
So in practice these are

    ( 0, 1 )
    -- ( ( [ "0" ], "number" ), ( [ "1" ], "number" ) )
    ( identity, List.map identity )
    -- ( ( [ "0" ], "a" ) -> ( [ "0" ], "a" )
    -- , List ( [ "1", "argument0" ], "a" )
    --   -> List ( [ "1", "argument0" ], "a" )
    -- )

`"0"` and `"1"` referring to the tuple part location
and `"argument0"` referring to the applied argument index.

We could work with some kind of name disambiguation system
but preserving names and context is usually nicer
for the final inferred variable names.

Why would you care?
[Types](#Type) inferred from [`expressionDeclaration`](#expressionDeclaration)
or [`expressionDeclarations`](#expressionDeclarations)
still contain these [`TypeVariableFromContext`](#TypeVariableFromContext)s
instead of just strings to preserve all that juicy information.
So you need to convert these yourself, like with something like

    typeVariableFromContextToString : ElmSyntaxTypeInfer.TypeVariableFromContext -> String
    typeVariableFromContextToString ( context, name ) =
        -- make sure the constraint like number is preserved
        name ++ (context |> List.map (\part -> "_" ++ part) |> String.concat)

(If you'd like to see something like this exposed,
[open an issue](https://github.com/lue-bird/elm-syntax-type-infer/issues/new))
        
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


typeVariableFromContextName : TypeVariableFromContext -> String
typeVariableFromContextName ( _, name ) =
    name


typeVariableConstraint : String -> Maybe TypeVariableConstraint
typeVariableConstraint variableName =
    if variableName |> String.startsWith "number" then
        Just TypeVariableConstraintNumber

    else if variableName |> String.startsWith "appendable" then
        Just TypeVariableConstraintAppendable

    else if variableName |> String.startsWith "comparable" then
        Just TypeVariableConstraintComparable

    else if variableName |> String.startsWith "compappend" then
        Just TypeVariableConstraintCompappend

    else
        Nothing


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


typeMapVariables :
    (variable -> variableMapped)
    -> Type variable
    -> Type variableMapped
typeMapVariables variableMap type_ =
    -- IGNORE TCO
    case type_ of
        TypeVariable variable ->
            TypeVariable (variable |> variableMap)

        TypeNotVariable typeNotVariable ->
            TypeNotVariable
                (typeNotVariable
                    |> typeNotVariableMapVariables variableMap
                )


typeNotVariableMapVariables :
    (variable -> variableMapped)
    -> TypeNotVariable variable
    -> TypeNotVariable variableMapped
typeNotVariableMapVariables variableMap typeNotVariable =
    case typeNotVariable of
        TypeUnit ->
            TypeUnit

        TypeConstruct typeConstruct ->
            TypeConstruct
                { moduleOrigin = typeConstruct.moduleOrigin
                , name = typeConstruct.name
                , arguments =
                    typeConstruct.arguments
                        |> List.map (\arg -> arg |> typeMapVariables variableMap)
                }

        TypeTuple typeTuple ->
            TypeTuple
                { part0 = typeTuple.part0 |> typeMapVariables variableMap
                , part1 = typeTuple.part1 |> typeMapVariables variableMap
                }

        TypeTriple typeTriple ->
            TypeTriple
                { part0 = typeTriple.part0 |> typeMapVariables variableMap
                , part1 = typeTriple.part1 |> typeMapVariables variableMap
                , part2 = typeTriple.part2 |> typeMapVariables variableMap
                }

        TypeRecord typeRecordFields ->
            TypeRecord
                (typeRecordFields
                    |> FastDict.map
                        (\_ fieldValue ->
                            fieldValue |> typeMapVariables variableMap
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
                                fieldValue |> typeMapVariables variableMap
                            )
                }

        TypeFunction typeFunction ->
            TypeFunction
                { input = typeFunction.input |> typeMapVariables variableMap
                , output = typeFunction.output |> typeMapVariables variableMap
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


typeSubstituteVariable :
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
typeSubstituteVariable declarationTypes replacement type_ =
    case replacement.type_ of
        TypeVariable argumentVariable ->
            Ok
                { type_ =
                    type_
                        |> typeMapVariables
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
                case replacement.variable |> typeVariableFromContextName |> typeVariableConstraint of
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
                                if replacement.type_ |> typeNotVariableIsComparable (\var -> var |> typeVariableFromContextName |> typeVariableConstraint) then
                                    Ok
                                        { type_ = TypeNotVariable replacement.type_
                                        , substitutions = variableSubstitutionsNone
                                        }

                                else
                                    Err "cannot unify comparable type variable with types other than Int/Float/String/Time.Posix/List of comparable/tuple of comparables/triple of comparable"

                            TypeVariableConstraintCompappend ->
                                if replacement.type_ |> typeNotVariableIsCompappend (\var -> var |> typeVariableFromContextName |> typeVariableConstraint) then
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
            resultAndThen2
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

        TypeTriple typeTriple ->
            resultAndThen3
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
                                    typeRecordExtensionUnifyWithRecord declarationTypes
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
            resultAndThen2
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
                                                            |> equivalentVariablesMergeWithSetOf2
                                                                variable
                                                                abUnifiedVariable
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


variableSubstitutionsMerge5 :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> VariableSubstitutions
    -> Result String VariableSubstitutions
variableSubstitutionsMerge5 declarationTypes a b c d e =
    variableSubstitutionsMerge4
        declarationTypes
        a
        b
        c
        d
        |> Result.andThen
            (\abcSubstitutions ->
                variableSubstitutionsMerge
                    declarationTypes
                    abcSubstitutions
                    e
            )


equivalentVariablesMergeWithSetOf2 :
    comparable
    -> comparable
    -> List (FastSet.Set comparable)
    -> List (FastSet.Set comparable)
equivalentVariablesMergeWithSetOf2 aEquivalentVariable bEquivalentVariable equivalentVariables =
    -- TODO optimize
    equivalentVariableSetMerge
        equivalentVariables
        [ FastSet.singleton aEquivalentVariable
            |> FastSet.insert bEquivalentVariable
        ]


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


typeUnify3 :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> Type TypeVariableFromContext
    -> Type TypeVariableFromContext
    -> Type TypeVariableFromContext
    ->
        Result
            String
            { type_ : Type TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeUnify3 declarationTypes a b c =
    typeUnify declarationTypes a b
        |> Result.andThen
            (\abUnified ->
                typeUnify declarationTypes
                    abUnified.type_
                    c
                    |> Result.andThen
                        (\abcUnified ->
                            variableSubstitutionsMerge declarationTypes
                                abUnified.substitutions
                                abcUnified.substitutions
                                |> Result.map
                                    (\fullSubstitutions ->
                                        { substitutions = fullSubstitutions
                                        , type_ = abcUnified.type_
                                        }
                                    )
                        )
            )


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
    let
        maybeTypeConstructsWithSameName :
            Maybe
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , name : String
                , aArguments : List (Type TypeVariableFromContext)
                , bArguments : List (Type TypeVariableFromContext)
                }
        maybeTypeConstructsWithSameName =
            case a of
                TypeConstruct aTypeConstruct ->
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
                                Just
                                    { moduleOrigin = aTypeConstruct.moduleOrigin
                                    , name = aTypeConstruct.name
                                    , aArguments = aTypeConstruct.arguments
                                    , bArguments = bTypeConstruct.arguments
                                    }

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        maybeUnifiedWithTypeConstruct :
            Maybe
                (Result
                    String
                    { type_ : Type TypeVariableFromContext
                    , substitutions : VariableSubstitutions
                    }
                )
        maybeUnifiedWithTypeConstruct =
            case maybeTypeConstructsWithSameName of
                Just matchingTypeConstructs ->
                    Result.map
                        (\argumentsABUnified ->
                            { type_ =
                                TypeNotVariable
                                    (TypeConstruct
                                        { moduleOrigin = matchingTypeConstructs.moduleOrigin
                                        , name = matchingTypeConstructs.name
                                        , arguments =
                                            argumentsABUnified.argumentsReverse
                                                |> List.reverse
                                        }
                                    )
                            , substitutions = argumentsABUnified.substitutions
                            }
                        )
                        (List.map2
                            (\aArgument bArgument -> { a = aArgument, b = bArgument })
                            matchingTypeConstructs.aArguments
                            matchingTypeConstructs.bArguments
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
                        )
                        |> Just

                Nothing ->
                    case typeUnifyWithTryToExpandTypeConstruct declarationTypes a b of
                        Just result ->
                            Just result

                        Nothing ->
                            typeUnifyWithTryToExpandTypeConstruct declarationTypes b a
    in
    case maybeUnifiedWithTypeConstruct of
        Just result ->
            result

        Nothing ->
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
                    Err
                        ("choice type "
                            ++ qualifiedToString
                                { qualification = aTypeConstruct.moduleOrigin
                                , name = aTypeConstruct.name
                                }
                            ++ "cannot be unified be with a choice type with a different name"
                        )

                TypeTuple aTuple ->
                    case b of
                        TypeTuple bTuple ->
                            resultAndThen2
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

                        _ ->
                            Err "tuple (`( ..., ... )`) cannot be unified with types other than tuple"

                TypeTriple aTriple ->
                    case b of
                        TypeTriple bTriple ->
                            resultAndThen3
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
                            typeRecordExtensionUnifyWithRecord declarationTypes
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
                    case b of
                        TypeRecord bRecord ->
                            typeRecordExtensionUnifyWithRecord declarationTypes
                                aRecordExtension
                                bRecord
                                |> Result.map
                                    (\typeAndSubstitutions ->
                                        { type_ = TypeNotVariable typeAndSubstitutions.type_
                                        , substitutions = typeAndSubstitutions.substitutions
                                        }
                                    )

                        TypeRecordExtension bRecordExtension ->
                            typeRecordExtensionUnifyWithRecordExtension declarationTypes
                                aRecordExtension
                                bRecordExtension
                                |> Result.map
                                    (\typeAndSubstitutions ->
                                        { type_ = TypeNotVariable typeAndSubstitutions.type_
                                        , substitutions = typeAndSubstitutions.substitutions
                                        }
                                    )

                        _ ->
                            Err "record extension cannot be unified with types other than record or record extension"

                TypeFunction aFunction ->
                    case b of
                        TypeFunction bFunction ->
                            resultAndThen2
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

                        _ ->
                            Err "function (`... -> ...`) cannot be unified with types other than function"


typeUnifyWithTryToExpandTypeConstruct :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> TypeNotVariable TypeVariableFromContext
    -> TypeNotVariable TypeVariableFromContext
    ->
        Maybe
            (Result
                String
                { substitutions : VariableSubstitutions
                , type_ : Type TypeVariableFromContext
                }
            )
typeUnifyWithTryToExpandTypeConstruct declarationTypes toExpand b =
    case toExpand of
        TypeConstruct typeConstructToExpand ->
            case declarationTypes |> FastDict.get typeConstructToExpand.moduleOrigin of
                Nothing ->
                    Nothing

                Just aOriginModuleTypes ->
                    case aOriginModuleTypes.typeAliases |> FastDict.get typeConstructToExpand.name of
                        Just originAliasDeclaration ->
                            List.map2
                                (\parameterName argument ->
                                    { variable = ( [], parameterName ), type_ = argument }
                                )
                                originAliasDeclaration.parameters
                                typeConstructToExpand.arguments
                                |> listFoldlWhileOkFrom
                                    { type_ =
                                        originAliasDeclaration.type_
                                            |> typeMapVariables (\aliasVariable -> ( [], aliasVariable ))
                                    , substitutions = variableSubstitutionsNone
                                    }
                                    (\substitution soFar ->
                                        soFar.type_
                                            |> typeSubstituteVariable declarationTypes
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
                                |> Result.andThen
                                    (\typeConstructExpandedWithArguments ->
                                        typeUnify declarationTypes
                                            typeConstructExpandedWithArguments.type_
                                            (TypeNotVariable b)
                                            |> Result.andThen
                                                (\typeUnified ->
                                                    variableSubstitutionsMerge declarationTypes
                                                        typeConstructExpandedWithArguments.substitutions
                                                        typeUnified.substitutions
                                                        |> Result.map
                                                            (\fullSubstitutions ->
                                                                { substitutions = fullSubstitutions
                                                                , type_ = typeUnified.type_
                                                                }
                                                            )
                                                )
                                    )
                                |> Just

                        Nothing ->
                            Nothing

        _ ->
            Nothing


typeRecordUnify :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> FastDict.Dict String (Type TypeVariableFromContext)
    -> FastDict.Dict String (Type TypeVariableFromContext)
    ->
        Result
            String
            { type_ : TypeNotVariable TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeRecordUnify declarationTypes aFields bFields =
    Result.map
        (\fieldsUnified ->
            { type_ = TypeRecord fieldsUnified.fieldsUnified
            , substitutions = fieldsUnified.substitutions
            }
        )
        (FastDict.merge
            (\name _ _ ->
                Err
                    ("record with the field "
                        ++ name
                        ++ " cannot be unified with a record that does not have this field"
                    )
            )
            (\name aValue bValue soFarOrError ->
                resultAndThen2
                    (\abValueUnified soFar ->
                        variableSubstitutionsMerge declarationTypes
                            abValueUnified.substitutions
                            soFar.substitutions
                            |> Result.map
                                (\substitutionsWithField ->
                                    { substitutions = substitutionsWithField
                                    , fieldsUnified =
                                        soFar.fieldsUnified
                                            |> FastDict.insert name abValueUnified.type_
                                    }
                                )
                    )
                    (typeUnify declarationTypes aValue bValue)
                    soFarOrError
            )
            (\name _ _ ->
                Err
                    ("record with the field "
                        ++ name
                        ++ " cannot be unified with a record that does not have this field"
                    )
            )
            aFields
            bFields
            (Ok
                { fieldsUnified = FastDict.empty
                , substitutions = variableSubstitutionsNone
                }
            )
        )


typeRecordExtensionUnifyWithRecord :
    ModuleLevelDeclarationTypesInAvailableInModule
    ->
        { recordVariable : TypeVariableFromContext
        , fields : FastDict.Dict String (Type TypeVariableFromContext)
        }
    -> FastDict.Dict String (Type TypeVariableFromContext)
    ->
        Result
            String
            { type_ : TypeNotVariable TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeRecordExtensionUnifyWithRecord declarationTypes recordExtension recordFields =
    Result.andThen
        (\fieldsUnified ->
            Result.map
                (\fullSubstitutions ->
                    { substitutions = fullSubstitutions
                    , type_ =
                        TypeRecordExtension
                            { recordVariable = recordExtension.recordVariable
                            , fields = fieldsUnified.fieldsUnified
                            }
                    }
                )
                (variableSubstitutionsMerge declarationTypes
                    fieldsUnified.substitutions
                    { equivalentVariables = []
                    , variableToType =
                        FastDict.singleton
                            recordExtension.recordVariable
                            (TypeRecord
                                (FastDict.diff
                                    recordFields
                                    recordExtension.fields
                                )
                            )
                    }
                )
        )
        (FastDict.merge
            (\name _ _ ->
                Err
                    ("record extension with the field "
                        ++ name
                        ++ " cannot be unified with a record that does not have this field"
                    )
            )
            (\name aValue bValue soFarOrError ->
                resultAndThen2
                    (\abValueUnified soFar ->
                        variableSubstitutionsMerge declarationTypes
                            abValueUnified.substitutions
                            soFar.substitutions
                            |> Result.map
                                (\substitutionsWithField ->
                                    { substitutions = substitutionsWithField
                                    , fieldsUnified =
                                        soFar.fieldsUnified
                                            |> FastDict.insert name abValueUnified.type_
                                    }
                                )
                    )
                    (typeUnify declarationTypes aValue bValue)
                    soFarOrError
            )
            (\name value soFarOrError ->
                soFarOrError
                    |> Result.map
                        (\soFar ->
                            { substitutions = soFar.substitutions
                            , fieldsUnified =
                                soFar.fieldsUnified
                                    |> FastDict.insert name value
                            }
                        )
            )
            recordExtension.fields
            recordFields
            (Ok
                { fieldsUnified = FastDict.empty
                , substitutions = variableSubstitutionsNone
                }
            )
        )


typeRecordExtensionUnifyWithRecordExtension :
    ModuleLevelDeclarationTypesInAvailableInModule
    ->
        { recordVariable : TypeVariableFromContext
        , fields : FastDict.Dict String (Type TypeVariableFromContext)
        }
    ->
        { recordVariable : TypeVariableFromContext
        , fields : FastDict.Dict String (Type TypeVariableFromContext)
        }
    ->
        Result
            String
            { type_ : TypeNotVariable TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
typeRecordExtensionUnifyWithRecordExtension declarationTypes aRecordExtension bRecordExtension =
    Result.andThen
        (\fieldsUnified ->
            let
                newBaseVariable : TypeVariableFromContext
                newBaseVariable =
                    ( -- creating a new variable safely
                      "_of"
                        :: ([ (aRecordExtension.recordVariable |> typeVariableFromContextName)
                                :: (aRecordExtension.recordVariable |> Tuple.first)
                            , (bRecordExtension.recordVariable |> typeVariableFromContextName)
                                :: (bRecordExtension.recordVariable |> Tuple.first)
                            ]
                                |> List.sort
                                |> List.intersperse [ "_and" ]
                                |> List.concat
                           )
                    , "base"
                    )
            in
            Result.map
                (\fullSubstitutions ->
                    { substitutions = fullSubstitutions
                    , type_ =
                        TypeRecordExtension
                            { recordVariable = newBaseVariable
                            , fields = fieldsUnified.fieldsUnified
                            }
                    }
                )
                (variableSubstitutionsMerge declarationTypes
                    fieldsUnified.substitutions
                    { equivalentVariables = []
                    , variableToType =
                        FastDict.singleton
                            aRecordExtension.recordVariable
                            (TypeRecordExtension
                                { recordVariable = newBaseVariable
                                , fields =
                                    FastDict.diff
                                        bRecordExtension.fields
                                        aRecordExtension.fields
                                }
                            )
                            |> FastDict.insert
                                bRecordExtension.recordVariable
                                (TypeRecordExtension
                                    { recordVariable = newBaseVariable
                                    , fields =
                                        FastDict.diff
                                            aRecordExtension.fields
                                            bRecordExtension.fields
                                    }
                                )
                    }
                )
        )
        (FastDict.merge
            (\name value soFarOrError ->
                soFarOrError
                    |> Result.map
                        (\soFar ->
                            { substitutions = soFar.substitutions
                            , fieldsUnified =
                                soFar.fieldsUnified
                                    |> FastDict.insert name value
                            }
                        )
            )
            (\name aValue bValue soFarOrError ->
                resultAndThen2
                    (\abValueUnified soFar ->
                        variableSubstitutionsMerge declarationTypes
                            abValueUnified.substitutions
                            soFar.substitutions
                            |> Result.map
                                (\substitutionsWithField ->
                                    { substitutions = substitutionsWithField
                                    , fieldsUnified =
                                        soFar.fieldsUnified
                                            |> FastDict.insert name abValueUnified.type_
                                    }
                                )
                    )
                    (typeUnify declarationTypes aValue bValue)
                    soFarOrError
            )
            (\name value soFarOrError ->
                soFarOrError
                    |> Result.map
                        (\soFar ->
                            { substitutions = soFar.substitutions
                            , fieldsUnified =
                                soFar.fieldsUnified
                                    |> FastDict.insert name value
                            }
                        )
            )
            aRecordExtension.fields
            bRecordExtension.fields
            (Ok
                { fieldsUnified = FastDict.empty
                , substitutions = variableSubstitutionsNone
                }
            )
        )


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
but its sub-nodes are [`TypedNode`](#TypedNode)s
-}
type Expression
    = ExpressionUnit
    | ExpressionNumber
        { base : Base10Or16
        , value : Int
        }
    | ExpressionFloat Float
    | ExpressionString String
    | ExpressionChar Char
    | ExpressionReference
        { moduleOrigin :
            -- `[]` for current module
            Elm.Syntax.ModuleName.ModuleName
        , qualification :
            -- `[]` for no qualification
            Elm.Syntax.ModuleName.ModuleName
        , name : String
        }
    | ExpressionOperatorFunction String
    | ExpressionRecordAccessFunction String
    | ExpressionNegation (TypedNode Expression)
    | ExpressionParenthesized (TypedNode Expression)
    | ExpressionTuple
        { part0 : TypedNode Expression
        , part1 : TypedNode Expression
        }
    | ExpressionTriple
        { part0 : TypedNode Expression
        , part1 : TypedNode Expression
        , part2 : TypedNode Expression
        }
    | ExpressionRecordAccess
        { record : TypedNode Expression
        , fieldNameRange : Elm.Syntax.Range.Range
        , fieldName : String
        }
    | ExpressionInfixOperation
        { symbol : String
        , left : TypedNode Expression
        , right : TypedNode Expression
        }
    | ExpressionIfThenElse
        { condition : TypedNode Expression
        , onTrue : TypedNode Expression
        , onFalse : TypedNode Expression
        }
    | ExpressionList (List (TypedNode Expression))
    | ExpressionCall
        { called : TypedNode Expression
        , argument0 : TypedNode Expression
        , argument1Up : List (TypedNode Expression)
        }
    | ExpressionRecord
        (List
            { range : Elm.Syntax.Range.Range
            , name : String
            , nameRange : Elm.Syntax.Range.Range
            , value : TypedNode Expression
            }
        )
    | ExpressionRecordUpdate
        { recordVariable : TypedNode String
        , fields :
            List
                { range : Elm.Syntax.Range.Range
                , name : String
                , nameRange : Elm.Syntax.Range.Range
                , value : TypedNode Expression
                }
        }
    | ExpressionLambda
        -- TODO split into parameter0 and parameter1Up
        { arguments : List (TypedNode Pattern)
        , result : TypedNode Expression
        }
    | ExpressionLetIn
        { declarations : List (Elm.Syntax.Node.Node LetDeclaration)
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
            Maybe
                { nameRange : Elm.Syntax.Range.Range
                , type_ : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
                }
        , nameRange : Elm.Syntax.Range.Range
        , name : String
        , arguments : List (TypedNode Pattern)
        , result : TypedNode Expression
        , type_ : Type TypeVariableFromContext
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
        { moduleOrigin :
            -- `[]` for current module
            Elm.Syntax.ModuleName.ModuleName
        , qualification :
            -- `[]` for no qualification
            Elm.Syntax.ModuleName.ModuleName
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


typeBasicsBool : Type variable_
typeBasicsBool =
    TypeNotVariable
        (TypeConstruct
            { moduleOrigin = [ "Basics" ]
            , name = "Bool"
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

        Elm.Syntax.Pattern.ParenthesizedPattern parenthesizedInParens ->
            Result.map
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
                (parenthesizedInParens
                    |> patternTypeInfer context
                )

        Elm.Syntax.Pattern.AsPattern innerPatternNode (Elm.Syntax.Node.Node variableNameRange variableName) ->
            Result.map
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
                (innerPatternNode
                    |> patternTypeInfer context
                )

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
                    Result.map
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
                        (parenthesizedInParens
                            |> patternTypeInfer context
                        )

                [ tuplePart0, tuplePart1 ] ->
                    resultAndThen2
                        (\part0 part1 ->
                            Result.map
                                (\fullSubstitutions ->
                                    { node =
                                        { range = fullRange
                                        , value = PatternTuple { part0 = part0.node, part1 = part1.node }
                                        , type_ =
                                            TypeNotVariable
                                                (TypeTuple
                                                    { part0 = part0.node.type_, part1 = part1.node.type_ }
                                                )
                                        }
                                    , substitutions = fullSubstitutions
                                    , introducedExpressionVariables =
                                        FastDict.union
                                            part0.introducedExpressionVariables
                                            part1.introducedExpressionVariables
                                    }
                                )
                                (variableSubstitutionsMerge
                                    context.declarationTypes
                                    part0.substitutions
                                    part1.substitutions
                                )
                        )
                        (tuplePart0 |> patternTypeInfer context)
                        (tuplePart1 |> patternTypeInfer context)

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    resultAndThen3
                        (\part0 part1 part2 ->
                            Result.map
                                (\fullSubstitutions ->
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
                                    , substitutions = fullSubstitutions
                                    , introducedExpressionVariables =
                                        FastDict.union
                                            part0.introducedExpressionVariables
                                            (FastDict.union
                                                part1.introducedExpressionVariables
                                                part2.introducedExpressionVariables
                                            )
                                    }
                                )
                                (variableSubstitutionsMerge3
                                    context.declarationTypes
                                    part0.substitutions
                                    part1.substitutions
                                    part2.substitutions
                                )
                        )
                        (tuplePart0 |> patternTypeInfer context)
                        (tuplePart1 |> patternTypeInfer context)
                        (tuplePart2 |> patternTypeInfer context)

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
            resultAndThen2
                (\headInferred tailInferred ->
                    Result.andThen
                        (\fullListUnified ->
                            Result.map
                                (\fullSubstitutions ->
                                    { substitutions = fullSubstitutions
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
                                (variableSubstitutionsMerge3
                                    context.declarationTypes
                                    headInferred.substitutions
                                    tailInferred.substitutions
                                    fullListUnified.substitutions
                                )
                        )
                        (typeUnify
                            context.declarationTypes
                            (typeListList headInferred.node.type_)
                            tailInferred.node.type_
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
                    Result.map
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
                        (Result.andThen
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
                                            Result.andThen
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
                                                (patternTypeInfer
                                                    (context
                                                        |> patternContextToInPath
                                                            (soFar.index |> String.fromInt)
                                                    )
                                                    elementNode
                                                )
                                        )
                            )
                            (patternTypeInfer
                                (context |> patternContextToInPath "0")
                                head
                            )
                        )

        Elm.Syntax.Pattern.NamedPattern qualified arguments ->
            case context.moduleOriginLookup.references |> FastDict.get ( qualified.moduleName, qualified.name ) of
                Nothing ->
                    Err
                        ("no module origin found for the variant reference "
                            ++ qualifiedToString
                                { qualification = qualified.moduleName
                                , name = qualified.name
                                }
                        )

                Just moduleOrigin ->
                    case context.declarationTypes |> FastDict.get moduleOrigin of
                        Nothing ->
                            Err
                                ("no declaration types found at the module origin of the variant reference "
                                    ++ qualifiedToString
                                        { qualification = moduleOrigin
                                        , name = qualified.name
                                        }
                                )

                        Just moduleOriginDeclarationTypes ->
                            case
                                moduleOriginDeclarationTypes.choiceTypes
                                    |> fastDictMapAndSmallestJust
                                        (\choiceTypeName choiceTypeInfo ->
                                            choiceTypeInfo.variants
                                                |> FastDict.get qualified.name
                                                |> Maybe.map
                                                    (\variantParameters ->
                                                        { variantParameters = variantParameters
                                                        , choiceTypeName = choiceTypeName
                                                        , choiceTypeParameters = choiceTypeInfo.parameters
                                                        }
                                                    )
                                        )
                            of
                                Nothing ->
                                    Err
                                        ("no choice type found at the module origin with the variant reference "
                                            ++ qualifiedToString
                                                { qualification = moduleOrigin
                                                , name = qualified.name
                                                }
                                        )

                                Just variant ->
                                    patternVariantTypeInfer context
                                        { fullRange = fullRange
                                        , qualification = qualified.moduleName
                                        , moduleOrigin = moduleOrigin
                                        , name = qualified.name
                                        , variantParameters = variant.variantParameters
                                        , choiceTypeName = variant.choiceTypeName
                                        , choiceTypeParameters = variant.choiceTypeParameters
                                        , arguments = arguments
                                        }


patternVariantTypeInfer :
    { moduleOriginLookup : ModuleOriginLookup, path : List String, declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule }
    ->
        { fullRange : Elm.Syntax.Range.Range
        , moduleOrigin : Elm.Syntax.ModuleName.ModuleName
        , qualification : Elm.Syntax.ModuleName.ModuleName
        , name : String
        , choiceTypeName : String
        , choiceTypeParameters : List String
        , variantParameters : List (Type String)
        , arguments : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        }
    ->
        Result
            String
            { substitutions : VariableSubstitutions
            , introducedExpressionVariables :
                FastDict.Dict
                    String
                    (Type TypeVariableFromContext)
            , node : TypedNode Pattern
            }
patternVariantTypeInfer context patternVariant =
    Result.map
        (\argumentsUnified ->
            { substitutions = argumentsUnified.substitutions
            , introducedExpressionVariables =
                argumentsUnified.introducedExpressionVariables
            , node =
                { range = patternVariant.fullRange
                , value =
                    PatternVariant
                        { moduleOrigin = patternVariant.moduleOrigin
                        , qualification = patternVariant.qualification
                        , name = patternVariant.name
                        , arguments =
                            argumentsUnified.nodesReverse
                                |> List.reverse
                        }
                , type_ =
                    TypeNotVariable
                        (TypeConstruct
                            { moduleOrigin = patternVariant.moduleOrigin
                            , name = patternVariant.choiceTypeName
                            , arguments =
                                patternVariant.choiceTypeParameters
                                    |> List.map
                                        (\choiceTypeParameter ->
                                            TypeVariable
                                                ( context.path, choiceTypeParameter )
                                        )
                            }
                        )
                }
            }
        )
        (List.map2
            (\typeInVariant argumentPattern ->
                { typeInVariant = typeInVariant
                , pattern = argumentPattern
                }
            )
            patternVariant.variantParameters
            patternVariant.arguments
            |> listFoldlWhileOkFrom
                { substitutions = variableSubstitutionsNone
                , introducedExpressionVariables =
                    FastDict.empty
                , nodesReverse = []
                }
                (\argument soFar ->
                    Result.andThen
                        (\argumentPatternInferred ->
                            Result.andThen
                                (\argumentUnified ->
                                    Result.map
                                        (\substitutionsWithArgument ->
                                            { substitutions = substitutionsWithArgument
                                            , introducedExpressionVariables =
                                                FastDict.union
                                                    argumentPatternInferred.introducedExpressionVariables
                                                    soFar.introducedExpressionVariables
                                            , nodesReverse =
                                                (argumentPatternInferred.node
                                                    |> typedNodeReplaceTypeBy argumentUnified.type_
                                                )
                                                    :: soFar.nodesReverse
                                            }
                                        )
                                        (variableSubstitutionsMerge context.declarationTypes
                                            argumentPatternInferred.substitutions
                                            argumentUnified.substitutions
                                        )
                                )
                                (typeUnify context.declarationTypes
                                    argumentPatternInferred.node.type_
                                    (argument.typeInVariant
                                        |> typeMapVariables
                                            (\variableName -> ( context.path, variableName ))
                                    )
                                )
                        )
                        (argument.pattern
                            |> patternTypeInfer context
                        )
                )
        )


expressionTypeInfer :
    { declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
    , locallyIntroducedExpressionVariables :
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

        Elm.Syntax.Expression.PrefixOperator operator ->
            Result.map
                (\type_ ->
                    { node =
                        { range = fullRange
                        , value = ExpressionOperatorFunction operator
                        , type_ = type_
                        }
                    , substitutions = variableSubstitutionsNone
                    }
                )
                (operatorFunctionType
                    { path = context.path
                    , moduleOriginLookup = context.moduleOriginLookup
                    }
                    operator
                )

        Elm.Syntax.Expression.FunctionOrValue qualification name ->
            case context.moduleOriginLookup.references |> FastDict.get ( qualification, name ) of
                Just moduleOrigin ->
                    case context.declarationTypes |> FastDict.get moduleOrigin of
                        Nothing ->
                            Err
                                ("No declaration types found for the origin module "
                                    ++ (moduleOrigin |> moduleNameToString)
                                )

                        Just originModuleDeclarationTypes ->
                            case originModuleDeclarationTypes.signatures |> FastDict.get name of
                                Just signatureType ->
                                    Ok
                                        { substitutions = variableSubstitutionsNone
                                        , node =
                                            { range = fullRange
                                            , value =
                                                ExpressionReference
                                                    { qualification = qualification
                                                    , moduleOrigin = moduleOrigin
                                                    , name = name
                                                    }
                                            , type_ =
                                                signatureType
                                                    |> typeMapVariables
                                                        (\variableName -> ( context.path, variableName ))
                                            }
                                        }

                                Nothing ->
                                    case
                                        originModuleDeclarationTypes.choiceTypes
                                            |> fastDictMapAndSmallestJust
                                                (\choiceTypeName choiceTypeInfo ->
                                                    choiceTypeInfo.variants
                                                        |> FastDict.get name
                                                        |> Maybe.map
                                                            (\variantParameters ->
                                                                { variantParameters = variantParameters
                                                                , choiceTypeName = choiceTypeName
                                                                , choiceTypeParameters = choiceTypeInfo.parameters
                                                                }
                                                            )
                                                )
                                    of
                                        Just variant ->
                                            let
                                                resultType : Type String
                                                resultType =
                                                    TypeNotVariable
                                                        (TypeConstruct
                                                            { moduleOrigin = moduleOrigin
                                                            , name = variant.choiceTypeName
                                                            , arguments =
                                                                variant.choiceTypeParameters
                                                                    |> List.map TypeVariable
                                                            }
                                                        )

                                                fullType : Type TypeVariableFromContext
                                                fullType =
                                                    variant.variantParameters
                                                        |> List.foldr
                                                            (\argument output ->
                                                                TypeNotVariable
                                                                    (TypeFunction
                                                                        { input = argument
                                                                        , output = output
                                                                        }
                                                                    )
                                                            )
                                                            resultType
                                                        |> typeMapVariables
                                                            (\variableName -> ( context.path, variableName ))
                                            in
                                            Ok
                                                { substitutions = variableSubstitutionsNone
                                                , node =
                                                    { range = fullRange
                                                    , value =
                                                        ExpressionReference
                                                            { qualification = qualification
                                                            , moduleOrigin = moduleOrigin
                                                            , name = name
                                                            }
                                                    , type_ = fullType
                                                    }
                                                }

                                        Nothing ->
                                            case originModuleDeclarationTypes.typeAliases |> FastDict.get name of
                                                Just _ ->
                                                    Err
                                                        ("I found what looks like a record type alias constructor: "
                                                            ++ qualifiedToString
                                                                { qualification = moduleOrigin, name = name }
                                                            ++ ". These are not supported, yet because our record types don't preserve field order.\n"
                                                            ++ "Hint: no value/function/port/variant was found in the origin module of that reference, so that might be the actual problem."
                                                        )

                                                Nothing ->
                                                    Err
                                                        ("No value/function/port/variant/record type alias constructor found in the origin module of the reference "
                                                            ++ qualifiedToString
                                                                { qualification = moduleOrigin, name = name }
                                                        )

                Nothing ->
                    case qualification of
                        qualificationPart0 :: qualificationPart1Up ->
                            Err
                                ("No origin module found for the qualified reference "
                                    ++ qualifiedToString
                                        { qualification = qualificationPart0 :: qualificationPart1Up
                                        , name = name
                                        }
                                )

                        [] ->
                            case context.locallyIntroducedExpressionVariables |> FastDict.get name of
                                Nothing ->
                                    Err
                                        ("No locally introduced expression variables found with the name "
                                            ++ name
                                        )

                                Just locallyIntroducedExpressionVariable ->
                                    Ok
                                        { substitutions = variableSubstitutionsNone
                                        , node =
                                            { range = fullRange
                                            , value =
                                                ExpressionReference
                                                    { qualification = []
                                                    , moduleOrigin = []
                                                    , name = name
                                                    }
                                            , type_ = locallyIntroducedExpressionVariable
                                            }
                                        }

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

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            Result.map
                (\inParensNodeAndSubstitutions ->
                    { node =
                        { value = ExpressionParenthesized inParensNodeAndSubstitutions.node
                        , type_ = inParensNodeAndSubstitutions.node.type_
                        , range = fullRange
                        }
                    , substitutions = inParensNodeAndSubstitutions.substitutions
                    }
                )
                (inParens
                    |> expressionTypeInfer context
                )

        Elm.Syntax.Expression.Negation toNegate ->
            Result.andThen
                (\toNegateInferred ->
                    typeUnify context.declarationTypes
                        toNegateInferred.node.type_
                        (TypeVariable ( context.path, "numberNegated" ))
                        |> Result.andThen
                            (\fullType ->
                                variableSubstitutionsMerge context.declarationTypes
                                    toNegateInferred.substitutions
                                    fullType.substitutions
                                    |> Result.map
                                        (\fullSubstitutions ->
                                            { node =
                                                { value =
                                                    ExpressionNegation
                                                        { type_ = fullType.type_
                                                        , range = toNegateInferred.node.range
                                                        , value = toNegateInferred.node.value
                                                        }
                                                , type_ = fullType.type_
                                                , range = fullRange
                                                }
                                            , substitutions = fullSubstitutions
                                            }
                                        )
                            )
                )
                (toNegate
                    |> expressionTypeInfer context
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

        Elm.Syntax.Expression.OperatorApplication operator _ left right ->
            expressionInfixOperationTypeInfer context
                { fullRange = fullRange
                , operator = operator
                , left = left
                , right = right
                }

        Elm.Syntax.Expression.IfBlock condition onTrue onFalse ->
            resultAndThen3
                (\conditionInferred onTrueInferred onFalseInferred ->
                    resultAndThen2
                        (\conditionTypeInferredAsBool resultType ->
                            variableSubstitutionsMerge5 context.declarationTypes
                                conditionInferred.substitutions
                                onTrueInferred.substitutions
                                onFalseInferred.substitutions
                                conditionTypeInferredAsBool.substitutions
                                resultType.substitutions
                                |> Result.map
                                    (\fullSubstitutions ->
                                        { substitutions = fullSubstitutions
                                        , node =
                                            { range = fullRange
                                            , value =
                                                ExpressionIfThenElse
                                                    { condition =
                                                        { range = conditionInferred.node.range
                                                        , value = conditionInferred.node.value
                                                        , type_ = typeBasicsBool
                                                        }
                                                    , onTrue =
                                                        { range = onTrueInferred.node.range
                                                        , value = onTrueInferred.node.value
                                                        , type_ = resultType.type_
                                                        }
                                                    , onFalse =
                                                        { range = onTrueInferred.node.range
                                                        , value = onTrueInferred.node.value
                                                        , type_ = resultType.type_
                                                        }
                                                    }
                                            , type_ = resultType.type_
                                            }
                                        }
                                    )
                        )
                        (typeUnify context.declarationTypes
                            conditionInferred.node.type_
                            typeBasicsBool
                        )
                        (typeUnify context.declarationTypes
                            onTrueInferred.node.type_
                            onFalseInferred.node.type_
                        )
                )
                (condition |> expressionTypeInfer (context |> expressionContextToInPath "condition"))
                (onTrue |> expressionTypeInfer (context |> expressionContextToInPath "onTrue"))
                (onFalse |> expressionTypeInfer (context |> expressionContextToInPath "onFalse"))

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
                    resultAndThen2
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
                        (part0 |> expressionTypeInfer (context |> expressionContextToInPath "0"))
                        (part1 |> expressionTypeInfer (context |> expressionContextToInPath "1"))

                [ part0, part1, part2 ] ->
                    resultAndThen3
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
                        (part0 |> expressionTypeInfer (context |> expressionContextToInPath "0"))
                        (part1 |> expressionTypeInfer (context |> expressionContextToInPath "1"))
                        (part2 |> expressionTypeInfer (context |> expressionContextToInPath "2"))

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts. Should not exist in a valid parse result"

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
                    Result.map
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
                        (expressionTypeInfer
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
                        )

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "empty application is invalid syntax"

                [ subExpression ] ->
                    subExpression |> expressionTypeInfer context

                called :: argument0 :: argument1Up ->
                    resultAndThen3
                        (\calledInferred argument0Inferred argument1UpInferred ->
                            let
                                resultType : Type TypeVariableFromContext
                                resultType =
                                    TypeVariable ( context.path, "result" )

                                calledTypeInferredFromArguments : Type TypeVariableFromContext
                                calledTypeInferredFromArguments =
                                    TypeNotVariable
                                        (TypeFunction
                                            { input = argument0Inferred.node.type_
                                            , output =
                                                argument1UpInferred.nodesReverse
                                                    |> List.foldl
                                                        (\argumentInferred output ->
                                                            TypeNotVariable
                                                                (TypeFunction
                                                                    { input = argumentInferred.type_
                                                                    , output = output
                                                                    }
                                                                )
                                                        )
                                                        resultType
                                            }
                                        )
                            in
                            typeUnify context.declarationTypes
                                calledTypeInferredFromArguments
                                calledInferred.node.type_
                                |> Result.andThen
                                    (\callType ->
                                        variableSubstitutionsMerge4 context.declarationTypes
                                            calledInferred.substitutions
                                            argument0Inferred.substitutions
                                            argument1UpInferred.substitutions
                                            callType.substitutions
                                            |> Result.map
                                                (\fullSubstitutions ->
                                                    { substitutions = fullSubstitutions
                                                    , node =
                                                        { range = fullRange
                                                        , value =
                                                            ExpressionCall
                                                                { called = calledInferred.node
                                                                , argument0 = argument0Inferred.node
                                                                , argument1Up =
                                                                    argument1UpInferred.nodesReverse
                                                                        |> List.reverse
                                                                }
                                                        , type_ = resultType
                                                        }
                                                    }
                                                )
                                    )
                        )
                        (called
                            |> expressionTypeInfer
                                (context |> expressionContextToInPath "called")
                        )
                        (argument0
                            |> expressionTypeInfer
                                (context |> expressionContextToInPath "argument0")
                        )
                        (argument1Up
                            |> listFoldlWhileOkFrom
                                { substitutions = variableSubstitutionsNone
                                , nodesReverse = []
                                , index = 1
                                }
                                (\argumentNode soFar ->
                                    argumentNode
                                        |> expressionTypeInfer
                                            (context
                                                |> expressionContextToInPath
                                                    ("argument" ++ (soFar.index |> String.fromInt))
                                            )
                                        |> Result.andThen
                                            (\argumentInferred ->
                                                variableSubstitutionsMerge context.declarationTypes
                                                    argumentInferred.substitutions
                                                    soFar.substitutions
                                                    |> Result.map
                                                        (\substitutionsWithArgument ->
                                                            { index = soFar.index + 1
                                                            , substitutions = substitutionsWithArgument
                                                            , nodesReverse =
                                                                argumentInferred.node
                                                                    :: soFar.nodesReverse
                                                            }
                                                        )
                                            )
                                )
                        )

        Elm.Syntax.Expression.RecordExpr fields ->
            Result.map
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
                (fields
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
                                                        { range = fieldRange
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
                )

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node recordVariableRange recordVariable) fields ->
            resultAndThen2
                (\recordVariableInferred fieldsInferred ->
                    Result.andThen
                        (\typeUnified ->
                            Result.map
                                (\fullSubstitutions ->
                                    { substitutions = fullSubstitutions
                                    , node =
                                        { range = fullRange
                                        , value =
                                            ExpressionRecordUpdate
                                                { recordVariable =
                                                    { range = recordVariableRange
                                                    , value = recordVariable
                                                    , type_ = typeUnified.type_
                                                    }
                                                , fields =
                                                    fieldsInferred.nodesReverse
                                                        |> List.reverse
                                                }
                                        , type_ = typeUnified.type_
                                        }
                                    }
                                )
                                (variableSubstitutionsMerge context.declarationTypes
                                    recordVariableInferred.substitutions
                                    fieldsInferred.substitutions
                                )
                        )
                        (typeUnify context.declarationTypes
                            recordVariableInferred.node.type_
                            (TypeNotVariable
                                (TypeRecordExtension
                                    { recordVariable = ( context.path, "record" )
                                    , fields =
                                        fieldsInferred.nodesReverse
                                            |> List.foldl
                                                (\fieldInferred soFar ->
                                                    soFar
                                                        |> FastDict.insert fieldInferred.name
                                                            fieldInferred.value.type_
                                                )
                                                FastDict.empty
                                    }
                                )
                            )
                        )
                )
                (Elm.Syntax.Node.Node
                    recordVariableRange
                    (Elm.Syntax.Expression.FunctionOrValue [] recordVariable)
                    |> expressionTypeInfer
                        (context |> expressionContextToInPath "record")
                )
                (fields
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\(Elm.Syntax.Node.Node fieldRange ( Elm.Syntax.Node.Node nameRange name, valueNode )) soFar ->
                            Result.andThen
                                (\valueInferred ->
                                    Result.map
                                        (\substitutionsWithField ->
                                            { substitutions = substitutionsWithField
                                            , nodesReverse =
                                                { range = fieldRange
                                                , name = name
                                                , nameRange = nameRange
                                                , value = valueInferred.node
                                                }
                                                    :: soFar.nodesReverse
                                            }
                                        )
                                        (variableSubstitutionsMerge context.declarationTypes
                                            soFar.substitutions
                                            valueInferred.substitutions
                                        )
                                )
                                (valueNode
                                    |> expressionTypeInfer
                                        (context
                                            |> expressionContextToInPath
                                                ("field" ++ stringFirstCharToUpper name)
                                        )
                                )
                        )
                )

        Elm.Syntax.Expression.LambdaExpression lambda ->
            Result.andThen
                (\argumentsInferred ->
                    lambda.expression
                        |> expressionTypeInfer
                            { path = context.path
                            , declarationTypes = context.declarationTypes
                            , moduleOriginLookup = context.moduleOriginLookup
                            , locallyIntroducedExpressionVariables =
                                FastDict.union
                                    argumentsInferred.introducedExpressionVariables
                                    context.locallyIntroducedExpressionVariables
                            }
                        |> Result.andThen
                            (\resultInferred ->
                                variableSubstitutionsMerge context.declarationTypes
                                    argumentsInferred.substitutions
                                    resultInferred.substitutions
                                    |> Result.map
                                        (\fullSubstitutions ->
                                            { substitutions = fullSubstitutions
                                            , node =
                                                { range = fullRange
                                                , value =
                                                    ExpressionLambda
                                                        { arguments =
                                                            argumentsInferred.nodesReverse
                                                                |> List.reverse
                                                        , result = resultInferred.node
                                                        }
                                                , type_ =
                                                    argumentsInferred.nodesReverse
                                                        |> List.foldl
                                                            (\argumentTypedNode output ->
                                                                TypeNotVariable
                                                                    (TypeFunction
                                                                        { input = argumentTypedNode.type_
                                                                        , output = output
                                                                        }
                                                                    )
                                                            )
                                                            resultInferred.node.type_
                                                }
                                            }
                                        )
                            )
                )
                (lambda.args
                    |> listFoldlWhileOkFrom
                        { introducedExpressionVariables = FastDict.empty
                        , substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\argumentNode soFar ->
                            argumentNode
                                |> patternTypeInfer
                                    { path = context.path
                                    , declarationTypes = context.declarationTypes
                                    , moduleOriginLookup = context.moduleOriginLookup
                                    }
                                |> Result.andThen
                                    (\argumentInferred ->
                                        variableSubstitutionsMerge context.declarationTypes
                                            argumentInferred.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\substitutionsWithArgument ->
                                                    { substitutions = substitutionsWithArgument
                                                    , introducedExpressionVariables =
                                                        FastDict.union
                                                            argumentInferred.introducedExpressionVariables
                                                            soFar.introducedExpressionVariables
                                                    , nodesReverse =
                                                        argumentInferred.node
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )

        Elm.Syntax.Expression.CaseExpression caseOf ->
            resultAndThen2
                (\matchedInferred casesInferred ->
                    casesInferred.nodesReverse
                        |> listFoldlWhileOkFrom
                            { substitutions = variableSubstitutionsNone
                            , type_ = matchedInferred.node.type_
                            }
                            (\caseInferred soFar ->
                                typeUnify3 context.declarationTypes
                                    caseInferred.pattern.type_
                                    caseInferred.result.type_
                                    soFar.type_
                                    |> Result.andThen
                                        (\typeUnifiedWithCase ->
                                            variableSubstitutionsMerge context.declarationTypes
                                                typeUnifiedWithCase.substitutions
                                                soFar.substitutions
                                                |> Result.map
                                                    (\substitutionsWithCase ->
                                                        { substitutions = substitutionsWithCase
                                                        , type_ = typeUnifiedWithCase.type_
                                                        }
                                                    )
                                        )
                            )
                        |> Result.andThen
                            (\unifiedType ->
                                variableSubstitutionsMerge3 context.declarationTypes
                                    matchedInferred.substitutions
                                    casesInferred.substitutions
                                    unifiedType.substitutions
                                    |> Result.map
                                        (\fullSubstitutions ->
                                            { substitutions = fullSubstitutions
                                            , node =
                                                { range = fullRange
                                                , value =
                                                    ExpressionCaseOf
                                                        { matchedExpression =
                                                            matchedInferred.node
                                                                |> typedNodeReplaceTypeBy
                                                                    unifiedType.type_
                                                        , cases =
                                                            casesInferred.nodesReverse
                                                                |> listReverseAndMap
                                                                    (\case_ ->
                                                                        { pattern =
                                                                            case_.pattern
                                                                                |> typedNodeReplaceTypeBy
                                                                                    unifiedType.type_
                                                                        , result =
                                                                            case_.result
                                                                                |> typedNodeReplaceTypeBy
                                                                                    unifiedType.type_
                                                                        }
                                                                    )
                                                        }
                                                , type_ = unifiedType.type_
                                                }
                                            }
                                        )
                            )
                )
                (caseOf.expression
                    |> expressionTypeInfer context
                )
                (caseOf.cases
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\( casePattern, caseResult ) soFar ->
                            Result.andThen
                                (\patternInferred ->
                                    Result.andThen
                                        (\resultInferred ->
                                            variableSubstitutionsMerge3 context.declarationTypes
                                                patternInferred.substitutions
                                                resultInferred.substitutions
                                                soFar.substitutions
                                                |> Result.map
                                                    (\fullSubstitutions ->
                                                        { substitutions = fullSubstitutions
                                                        , nodesReverse =
                                                            { pattern = patternInferred.node
                                                            , result = resultInferred.node
                                                            }
                                                                :: soFar.nodesReverse
                                                        }
                                                    )
                                        )
                                        (caseResult
                                            |> expressionTypeInfer
                                                { declarationTypes = context.declarationTypes
                                                , moduleOriginLookup = context.moduleOriginLookup
                                                , path = context.path
                                                , locallyIntroducedExpressionVariables =
                                                    FastDict.union
                                                        patternInferred.introducedExpressionVariables
                                                        context.locallyIntroducedExpressionVariables
                                                }
                                        )
                                )
                                (casePattern
                                    |> patternTypeInfer
                                        { declarationTypes = context.declarationTypes
                                        , moduleOriginLookup = context.moduleOriginLookup
                                        , path = context.path
                                        }
                                )
                        )
                )

        Elm.Syntax.Expression.LetExpression letIn ->
            Debug.todo "branch 'LetExpression _' not implemented"

        Elm.Syntax.Expression.Operator _ ->
            Err "Elm.Syntax.Expression.Operator should not exist in a valid parse result"

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl shader expressions not supported"


moduleNameToString : Elm.Syntax.ModuleName.ModuleName -> String
moduleNameToString moduleName =
    moduleName |> String.join "."


typedNodeReplaceTypeBy :
    Type TypeVariableFromContext
    -> TypedNode value
    -> TypedNode value
typedNodeReplaceTypeBy replacementType typedNode =
    { value = typedNode.value
    , range = typedNode.range
    , type_ = replacementType
    }


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


expressionInfixOperationTypeInfer :
    { declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
    , locallyIntroducedExpressionVariables :
        FastDict.Dict String (Type TypeVariableFromContext)
    , path : List String
    , moduleOriginLookup : ModuleOriginLookup
    }
    ->
        { fullRange : Elm.Syntax.Range.Range
        , operator : String
        , left : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , right : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    ->
        Result
            String
            { substitutions : VariableSubstitutions
            , node : TypedNode Expression
            }
expressionInfixOperationTypeInfer context infixOperation =
    resultAndThen3
        (\operatorAsFunctionType leftInferred rightInferred ->
            let
                resultType : Type TypeVariableFromContext
                resultType =
                    TypeVariable ( context.path, "result" )
            in
            typeUnify context.declarationTypes
                operatorAsFunctionType
                (TypeNotVariable
                    (TypeFunction
                        { input = leftInferred.node.type_
                        , output =
                            TypeNotVariable
                                (TypeFunction
                                    { input = rightInferred.node.type_
                                    , output = resultType
                                    }
                                )
                        }
                    )
                )
                |> Result.andThen
                    (\unifiedType ->
                        variableSubstitutionsMerge3 context.declarationTypes
                            leftInferred.substitutions
                            rightInferred.substitutions
                            unifiedType.substitutions
                            |> Result.map
                                (\fullSubstitutions ->
                                    { substitutions = fullSubstitutions
                                    , node =
                                        { range = infixOperation.fullRange
                                        , value =
                                            ExpressionInfixOperation
                                                { symbol = infixOperation.operator
                                                , left = leftInferred.node
                                                , right = rightInferred.node
                                                }
                                        , type_ = resultType
                                        }
                                    }
                                )
                    )
        )
        (operatorFunctionType
            { path = context.path
            , moduleOriginLookup = context.moduleOriginLookup
            }
            infixOperation.operator
        )
        (infixOperation.left
            |> expressionTypeInfer context
        )
        (infixOperation.right
            |> expressionTypeInfer context
        )


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
        , locallyIntroducedExpressionVariables :
            FastDict.Dict String (Type TypeVariableFromContext)
        , path : List String
        , moduleOriginLookup : ModuleOriginLookup
        }
    ->
        { declarationTypes : ModuleLevelDeclarationTypesInAvailableInModule
        , locallyIntroducedExpressionVariables :
            FastDict.Dict String (Type TypeVariableFromContext)
        , path : List String
        , moduleOriginLookup : ModuleOriginLookup
        }
expressionContextToInPath innermostPathDescription context =
    { declarationTypes = context.declarationTypes
    , locallyIntroducedExpressionVariables = context.locallyIntroducedExpressionVariables
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
            , type_ : Type TypeVariableFromContext
            }
expressionDeclaration typesAndOriginLookup syntaxDeclarationExpression =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationExpression.declaration |> Elm.Syntax.Node.value
    in
    Result.andThen
        (\arguments ->
            let
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
                                                            |> typeMapVariables
                                                                (\( _, variableName ) -> variableName)
                                                        )
                                        }
                                   )
                            )
            in
            Result.andThen
                (\resultInferred ->
                    case syntaxDeclarationExpression.signature of
                        Nothing ->
                            Result.andThen
                                (\argumentAndResultSubstitutions ->
                                    { result = resultInferred.node
                                    , type_ = type_
                                    , arguments =
                                        arguments.nodesReverse |> List.reverse
                                    }
                                        |> declarationValueOrFunctionSubstituteVariablesByNotVariables
                                            declarationTypes
                                            argumentAndResultSubstitutions
                                )
                                (variableSubstitutionsMerge declarationTypes
                                    resultInferred.substitutions
                                    arguments.substitutions
                                )

                        Just (Elm.Syntax.Node.Node _ signature) ->
                            Result.andThen
                                (\typeUnifiedWithSignatureType ->
                                    Result.andThen
                                        (\argumentAndResultAndTypeUnifySubstitutions ->
                                            { result = resultInferred.node
                                            , type_ = typeUnifiedWithSignatureType.type_
                                            , arguments =
                                                arguments.nodesReverse |> List.reverse
                                            }
                                                |> declarationValueOrFunctionSubstituteVariablesByNotVariables
                                                    declarationTypes
                                                    argumentAndResultAndTypeUnifySubstitutions
                                        )
                                        (variableSubstitutionsMerge3 declarationTypes
                                            resultInferred.substitutions
                                            arguments.substitutions
                                            typeUnifiedWithSignatureType.substitutions
                                        )
                                )
                                (signature.typeAnnotation
                                    |> Elm.Syntax.Node.value
                                    |> syntaxToType typesAndOriginLookup.moduleOriginLookup
                                    |> Result.andThen
                                        (\signatureType ->
                                            typeUnify declarationTypes
                                                type_
                                                (signatureType
                                                    |> typeMapVariables
                                                        (\variable -> ( [], variable ))
                                                )
                                        )
                                )
                )
                (implementation.expression
                    |> expressionTypeInfer
                        { declarationTypes = declarationTypes
                        , locallyIntroducedExpressionVariables = arguments.introducedExpressionVariables
                        , path = [ "result" ]
                        , moduleOriginLookup = typesAndOriginLookup.moduleOriginLookup
                        }
                )
        )
        (implementation.arguments
            |> parameterPatternsTypeInfer
                { declarationTypes =
                    typesAndOriginLookup.importedTypes
                        |> FastDict.insert [] typesAndOriginLookup.otherModuleDeclaredTypes
                , path = []
                , moduleOriginLookup = typesAndOriginLookup.moduleOriginLookup
                }
        )


declarationValueOrFunctionCondenseEquivalentVariables :
    { equivalentVariables : List (FastSet.Set TypeVariableFromContext)
    , arguments : List (TypedNode Pattern)
    , result : TypedNode Expression
    , type_ : Type TypeVariableFromContext
    }
    ->
        Result
            String
            { arguments : List (TypedNode Pattern)
            , result : TypedNode Expression
            , type_ : Type TypeVariableFromContext
            }
declarationValueOrFunctionCondenseEquivalentVariables declarationValueOrFunctionAndEquivalentVariables =
    Result.map
        (\lookupCondensedVariable ->
            { arguments = declarationValueOrFunctionAndEquivalentVariables.arguments
            , result = declarationValueOrFunctionAndEquivalentVariables.result
            , type_ = declarationValueOrFunctionAndEquivalentVariables.type_
            }
                |> declarationValueOrFunctionMapTypeVariables
                    (\variable ->
                        lookupCondensedVariable
                            |> FastDict.get variable
                            |> Maybe.withDefault variable
                    )
        )
        (createEquivalentVariablesToCondensedVariableLookup
            declarationValueOrFunctionAndEquivalentVariables.equivalentVariables
        )


createEquivalentVariablesToCondensedVariableLookup :
    List (FastSet.Set TypeVariableFromContext)
    -> Result String (FastDict.Dict TypeVariableFromContext TypeVariableFromContext)
createEquivalentVariablesToCondensedVariableLookup equivalentVariables =
    equivalentVariables
        |> listFoldlWhileOkFrom
            FastDict.empty
            (\equivalentVariableSet soFar ->
                Result.map
                    (\unifiedVariable ->
                        equivalentVariableSet
                            |> FastSet.foldl
                                (\variable soFarInSet ->
                                    soFarInSet
                                        |> FastDict.insert variable unifiedVariable
                                )
                                soFar
                    )
                    (equivalentVariablesCreateCondensedVariable equivalentVariableSet)
            )


declarationValueOrFunctionSubstituteVariablesByNotVariables :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> VariableSubstitutions
    ->
        { arguments : List (TypedNode Pattern)
        , result : TypedNode Expression
        , type_ : Type TypeVariableFromContext
        }
    ->
        Result
            String
            { arguments : List (TypedNode Pattern)
            , result : TypedNode Expression
            , type_ : Type TypeVariableFromContext
            }
declarationValueOrFunctionSubstituteVariablesByNotVariables declarationTypes variableSubstitutions declarationValueOrFunctionSoFar =
    case variableSubstitutions.equivalentVariables of
        [] ->
            case variableSubstitutions.variableToType |> FastDict.popMin of
                Nothing ->
                    { equivalentVariables = variableSubstitutions.equivalentVariables
                    , arguments = declarationValueOrFunctionSoFar.arguments
                    , result = declarationValueOrFunctionSoFar.result
                    , type_ =
                        -- creating the type only at this stage would be faster
                        declarationValueOrFunctionSoFar.type_
                    }
                        |> declarationValueOrFunctionCondenseEquivalentVariables

                Just ( ( replacementVariable, replacementTypeNotVariable ), remainingReplacements ) ->
                    case
                        declarationValueOrFunctionSoFar
                            |> declarationValueOrFunctionSubstituteVariableByNotVariable
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
                                    declarationValueOrFunctionSubstituteVariablesByNotVariables
                                        declarationTypes
                                        substitutionsAfterSubstitution
                                        { arguments = substituted.arguments
                                        , result = substituted.result
                                        , type_ = substituted.type_
                                        }

        equivalentVariableSet0 :: equivalentVariableSet1Up ->
            case
                createEquivalentVariablesToCondensedVariableLookup
                    (equivalentVariableSet0 :: equivalentVariableSet1Up)
            of
                Err error ->
                    Err error

                Ok variableToCondensedLookup ->
                    case
                        variableSubstitutions.variableToType
                            |> fastDictFoldlWhileOkFrom
                                variableSubstitutionsNone
                                (\uncondensedVariable replacementType soFar ->
                                    case variableToCondensedLookup |> FastDict.get uncondensedVariable of
                                        Nothing ->
                                            Ok
                                                { equivalentVariables = soFar.equivalentVariables
                                                , variableToType =
                                                    soFar.variableToType
                                                        |> FastDict.insert uncondensedVariable
                                                            (replacementType
                                                                |> typeNotVariableMapVariables
                                                                    (\variable ->
                                                                        variableToCondensedLookup
                                                                            |> FastDict.get variable
                                                                            |> Maybe.withDefault variable
                                                                    )
                                                            )
                                                }

                                        Just condensedVariable ->
                                            let
                                                replacementTypeUsingCondensedVariables : TypeNotVariable TypeVariableFromContext
                                                replacementTypeUsingCondensedVariables =
                                                    replacementType
                                                        |> typeNotVariableMapVariables
                                                            (\variable ->
                                                                variableToCondensedLookup
                                                                    |> FastDict.get variable
                                                                    |> Maybe.withDefault variable
                                                            )
                                            in
                                            case soFar.variableToType |> FastDict.get condensedVariable of
                                                Nothing ->
                                                    Ok
                                                        { equivalentVariables = soFar.equivalentVariables
                                                        , variableToType =
                                                            soFar.variableToType
                                                                |> FastDict.insert condensedVariable
                                                                    replacementTypeUsingCondensedVariables
                                                        }

                                                Just existingReplacementTypeForCondensedVariable ->
                                                    Result.andThen
                                                        (\replacementTypeForCondensedVariable ->
                                                            variableSubstitutionsMerge declarationTypes
                                                                replacementTypeForCondensedVariable.substitutions
                                                                (case replacementTypeForCondensedVariable.type_ of
                                                                    TypeVariable newEquivalentVariable ->
                                                                        { equivalentVariables =
                                                                            soFar.equivalentVariables
                                                                                |> equivalentVariablesMergeWithSetOf2
                                                                                    condensedVariable
                                                                                    newEquivalentVariable
                                                                        , variableToType =
                                                                            soFar.variableToType
                                                                        }

                                                                    TypeNotVariable replacementTypeForCondensedVariableTypeNotVariable ->
                                                                        { equivalentVariables =
                                                                            soFar.equivalentVariables
                                                                        , variableToType =
                                                                            soFar.variableToType
                                                                                |> FastDict.insert condensedVariable
                                                                                    replacementTypeForCondensedVariableTypeNotVariable
                                                                        }
                                                                )
                                                        )
                                                        (typeNotVariableUnify declarationTypes
                                                            existingReplacementTypeForCondensedVariable
                                                            replacementTypeUsingCondensedVariables
                                                        )
                                )
                    of
                        Err error ->
                            Err error

                        Ok variableToTypeUsingCondensed ->
                            declarationValueOrFunctionSubstituteVariablesByNotVariables declarationTypes
                                variableToTypeUsingCondensed
                                (declarationValueOrFunctionSoFar
                                    |> declarationValueOrFunctionMapTypeVariables
                                        (\variable ->
                                            variableToCondensedLookup
                                                |> FastDict.get variable
                                                |> Maybe.withDefault variable
                                        )
                                )


declarationValueOrFunctionSubstituteVariableByNotVariable :
    ModuleLevelDeclarationTypesInAvailableInModule
    ->
        { variable : TypeVariableFromContext
        , type_ : TypeNotVariable TypeVariableFromContext
        }
    ->
        { arguments : List (TypedNode Pattern)
        , result : TypedNode Expression
        , type_ : Type TypeVariableFromContext
        }
    ->
        Result
            String
            { arguments : List (TypedNode Pattern)
            , result : TypedNode Expression
            , type_ : Type TypeVariableFromContext
            , substitutions : VariableSubstitutions
            }
declarationValueOrFunctionSubstituteVariableByNotVariable declarationTypes replacement declarationValueOrFunctionSoFar =
    resultAndThen3
        (\argumentsInferred resultInferred typeInferred ->
            Result.map
                (\fullSubstitutions ->
                    { arguments =
                        argumentsInferred.nodesReverse
                            |> List.reverse
                    , result = resultInferred.node
                    , type_ =
                        -- reconstructing the function is probably faster
                        typeInferred.type_
                    , substitutions = fullSubstitutions
                    }
                )
                (variableSubstitutionsMerge3 declarationTypes
                    argumentsInferred.substitutions
                    resultInferred.substitutions
                    typeInferred.substitutions
                )
        )
        (declarationValueOrFunctionSoFar.arguments
            |> listFoldlWhileOkFrom
                { substitutions = variableSubstitutionsNone
                , nodesReverse = []
                }
                (\patternTypedNode soFar ->
                    Result.andThen
                        (\patternSubstituted ->
                            Result.map
                                (\fullSubstitutions ->
                                    { substitutions = fullSubstitutions
                                    , nodesReverse =
                                        patternSubstituted.node
                                            :: soFar.nodesReverse
                                    }
                                )
                                (variableSubstitutionsMerge declarationTypes
                                    patternSubstituted.substitutions
                                    soFar.substitutions
                                )
                        )
                        (patternTypedNode
                            |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                                replacement
                        )
                )
        )
        (declarationValueOrFunctionSoFar.result
            |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                replacement
        )
        (declarationValueOrFunctionSoFar.type_
            |> typeSubstituteVariableByNotVariable declarationTypes
                replacement
        )


declarationValueOrFunctionMapTypeVariables :
    (TypeVariableFromContext -> TypeVariableFromContext)
    ->
        { arguments : List (TypedNode Pattern)
        , result : TypedNode Expression
        , type_ : Type TypeVariableFromContext
        }
    ->
        { arguments : List (TypedNode Pattern)
        , result : TypedNode Expression
        , type_ : Type TypeVariableFromContext
        }
declarationValueOrFunctionMapTypeVariables variableChange declarationValueOrFunctionSoFar =
    { arguments =
        declarationValueOrFunctionSoFar.arguments
            |> List.map
                (\argument ->
                    argument |> patternTypedNodeMapTypeVariables variableChange
                )
    , result =
        declarationValueOrFunctionSoFar.result
            |> expressionTypedNodeMapTypeVariables variableChange
    , type_ =
        -- reconstructing the function is probably faster
        declarationValueOrFunctionSoFar.type_
            |> typeMapVariables variableChange
    }


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
            resultAndThen3
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

        ExpressionTuple expressionTuple ->
            resultAndThen3
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

        ExpressionTriple expressionTriple ->
            resultAndThen4
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

        ExpressionIfThenElse expressionIfThenElse ->
            resultAndThen4
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

        ExpressionList expressionListElements ->
            resultAndThen2
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

        ExpressionCall expressionCall ->
            resultAndThen4
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
                                    (\argumentSubstituted ->
                                        variableSubstitutionsMerge
                                            declarationTypes
                                            argumentSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\fullSubstitutions ->
                                                    { substitutions = fullSubstitutions
                                                    , nodesReverse =
                                                        argumentSubstituted.node
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

        ExpressionRecord expressionRecordFields ->
            resultAndThen2
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
                                                        { range = fieldNode.range
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

        ExpressionRecordUpdate expressionRecordUpdate ->
            resultAndThen2
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
                                                        { range = fieldNode.range
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

        ExpressionLambda expressionLambda ->
            resultAndThen3
                (\argumentsSubstituted resultSubstituted typeSubstituted ->
                    variableSubstitutionsMerge3 declarationTypes
                        argumentsSubstituted.substitutions
                        resultSubstituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionLambda
                                            { arguments =
                                                argumentsSubstituted.nodesReverse
                                                    |> List.reverse
                                            , result = resultSubstituted.node
                                            }
                                    , type_ = typeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionLambda.arguments
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\argumentNode soFar ->
                            argumentNode
                                |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\argumentSubstituted ->
                                        variableSubstitutionsMerge
                                            declarationTypes
                                            argumentSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\fullSubstitutions ->
                                                    { substitutions = fullSubstitutions
                                                    , nodesReverse =
                                                        argumentSubstituted.node
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (expressionLambda.result
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        ExpressionCaseOf expressionCaseOf ->
            resultAndThen3
                (\matchedSubstituted casesSubstituted typeSubstituted ->
                    variableSubstitutionsMerge3 declarationTypes
                        matchedSubstituted.substitutions
                        casesSubstituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionCaseOf
                                            { cases =
                                                casesSubstituted.nodesReverse
                                                    |> List.reverse
                                            , matchedExpression = matchedSubstituted.node
                                            }
                                    , type_ = typeSubstituted.type_
                                    }
                                }
                            )
                )
                (expressionCaseOf.matchedExpression
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (expressionCaseOf.cases
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\case_ soFar ->
                            resultAndThen2
                                (\patternSubstituted resultSubstituted ->
                                    variableSubstitutionsMerge3 declarationTypes
                                        patternSubstituted.substitutions
                                        resultSubstituted.substitutions
                                        soFar.substitutions
                                        |> Result.map
                                            (\fullSubstitutions ->
                                                { substitutions = fullSubstitutions
                                                , nodesReverse =
                                                    { pattern = patternSubstituted.node
                                                    , result = resultSubstituted.node
                                                    }
                                                        :: soFar.nodesReverse
                                                }
                                            )
                                )
                                (case_.pattern
                                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                                        replacement
                                )
                                (case_.result
                                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                                        replacement
                                )
                        )
                )
                (expression.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        ExpressionLetIn expressionLetIn ->
            resultAndThen2
                (\declarationsSubstituted resultSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        resultSubstituted.substitutions
                        declarationsSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = expression.range
                                    , value =
                                        ExpressionLetIn
                                            { declarations =
                                                declarationsSubstituted.nodesReverse
                                                    |> List.reverse
                                            , result = resultSubstituted.node
                                            }
                                    , type_ = resultSubstituted.node.type_
                                    }
                                }
                            )
                )
                (expressionLetIn.declarations
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\(Elm.Syntax.Node.Node declarationRange letDeclaration) soFar ->
                            Result.andThen
                                (\declarationSubstituted ->
                                    variableSubstitutionsMerge declarationTypes
                                        declarationSubstituted.substitutions
                                        soFar.substitutions
                                        |> Result.map
                                            (\fullSubstitutions ->
                                                { substitutions = fullSubstitutions
                                                , nodesReverse =
                                                    Elm.Syntax.Node.Node declarationRange
                                                        declarationSubstituted.letDeclaration
                                                        :: soFar.nodesReverse
                                                }
                                            )
                                )
                                (letDeclaration
                                    |> expressionLetDeclarationSubstituteVariableByNotVariable declarationTypes
                                        replacement
                                )
                        )
                )
                (expressionLetIn.result
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )


expressionLetDeclarationSubstituteVariableByNotVariable :
    ModuleLevelDeclarationTypesInAvailableInModule
    ->
        { variable : TypeVariableFromContext
        , type_ : TypeNotVariable TypeVariableFromContext
        }
    -> LetDeclaration
    ->
        Result
            String
            { letDeclaration : LetDeclaration
            , substitutions : VariableSubstitutions
            }
expressionLetDeclarationSubstituteVariableByNotVariable declarationTypes replacement letDeclaration =
    case letDeclaration of
        LetDestructuring letDestructuring ->
            resultAndThen2
                (\patternSubstituted expressionSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        patternSubstituted.substitutions
                        expressionSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , letDeclaration =
                                    LetDestructuring
                                        { pattern = patternSubstituted.node
                                        , expression = expressionSubstituted.node
                                        }
                                }
                            )
                )
                (letDestructuring.pattern
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (letDestructuring.expression
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        LetValueOrFunction letValueOrFunction ->
            resultAndThen3
                (\argumentsSubstituted resultSubstituted typeSubstituted ->
                    variableSubstitutionsMerge3 declarationTypes
                        argumentsSubstituted.substitutions
                        resultSubstituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , letDeclaration =
                                    LetValueOrFunction
                                        { arguments =
                                            argumentsSubstituted.nodesReverse
                                                |> List.reverse
                                        , result = resultSubstituted.node
                                        , type_ = typeSubstituted.type_
                                        , signature = letValueOrFunction.signature
                                        , nameRange = letValueOrFunction.nameRange
                                        , name = letValueOrFunction.name
                                        }
                                }
                            )
                )
                (letValueOrFunction.arguments
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\argumentNode soFar ->
                            argumentNode
                                |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\argumentSubstituted ->
                                        variableSubstitutionsMerge
                                            declarationTypes
                                            argumentSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\fullSubstitutions ->
                                                    { substitutions = fullSubstitutions
                                                    , nodesReverse =
                                                        argumentSubstituted.node
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (letValueOrFunction.result
                    |> expressionTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (letValueOrFunction.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )


expressionTypedNodeMapTypeVariables :
    (TypeVariableFromContext -> TypeVariableFromContext)
    -> TypedNode Expression
    -> TypedNode Expression
expressionTypedNodeMapTypeVariables typeVariableChange expressionTypedNode =
    { range = expressionTypedNode.range
    , value =
        expressionTypedNode.value
            |> expressionMapTypeVariables typeVariableChange
    , type_ =
        expressionTypedNode.type_
            |> typeMapVariables typeVariableChange
    }


expressionMapTypeVariables :
    (TypeVariableFromContext -> TypeVariableFromContext)
    -> Expression
    -> Expression
expressionMapTypeVariables typeVariableChange expression =
    -- IGNORE TCO
    -- TODO potentially merge with expressionTypedNodeMapTypeVariables
    -- to save some duplicate work
    case expression of
        ExpressionUnit ->
            ExpressionUnit

        ExpressionFloat floatValue ->
            ExpressionFloat floatValue

        ExpressionChar charValue ->
            ExpressionChar charValue

        ExpressionString stringValue ->
            ExpressionString stringValue

        ExpressionNumber expressionNumber ->
            ExpressionNumber expressionNumber

        ExpressionReference reference ->
            ExpressionReference reference

        ExpressionOperatorFunction expressionOperatorFunction ->
            ExpressionOperatorFunction expressionOperatorFunction

        ExpressionRecordAccessFunction fieldName ->
            ExpressionRecordAccessFunction fieldName

        ExpressionNegation inNegation ->
            ExpressionNegation
                (inNegation
                    |> expressionTypedNodeMapTypeVariables typeVariableChange
                )

        ExpressionParenthesized inParens ->
            ExpressionParenthesized
                (inParens
                    |> expressionTypedNodeMapTypeVariables typeVariableChange
                )

        ExpressionRecordAccess expressionRecordAccess ->
            ExpressionRecordAccess
                { record =
                    expressionRecordAccess.record
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , fieldName = expressionRecordAccess.fieldName
                , fieldNameRange = expressionRecordAccess.fieldNameRange
                }

        ExpressionInfixOperation expressionInfixOperation ->
            ExpressionInfixOperation
                { symbol = expressionInfixOperation.symbol
                , left =
                    expressionInfixOperation.left
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , right =
                    expressionInfixOperation.right
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                }

        ExpressionTuple expressionTuple ->
            ExpressionTuple
                { part0 =
                    expressionTuple.part0
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , part1 =
                    expressionTuple.part1
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                }

        ExpressionTriple expressionTriple ->
            ExpressionTriple
                { part0 =
                    expressionTriple.part0
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , part1 =
                    expressionTriple.part1
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , part2 =
                    expressionTriple.part2
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                }

        ExpressionIfThenElse expressionIfThenElse ->
            ExpressionIfThenElse
                { condition =
                    expressionIfThenElse.condition
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , onTrue =
                    expressionIfThenElse.onTrue
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , onFalse =
                    expressionIfThenElse.onFalse
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                }

        ExpressionList expressionListElements ->
            ExpressionList
                (expressionListElements
                    |> List.map
                        (\element ->
                            element
                                |> expressionTypedNodeMapTypeVariables
                                    typeVariableChange
                        )
                )

        ExpressionCall expressionCall ->
            ExpressionCall
                { called =
                    expressionCall.called
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , argument0 =
                    expressionCall.argument0
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , argument1Up =
                    expressionCall.argument1Up
                        |> List.map
                            (\argument ->
                                argument
                                    |> expressionTypedNodeMapTypeVariables
                                        typeVariableChange
                            )
                }

        ExpressionRecord expressionRecordFields ->
            ExpressionRecord
                (expressionRecordFields
                    |> List.map
                        (\field ->
                            { range = field.range
                            , name = field.name
                            , nameRange = field.nameRange
                            , value =
                                field.value
                                    |> expressionTypedNodeMapTypeVariables
                                        typeVariableChange
                            }
                        )
                )

        ExpressionRecordUpdate expressionRecordUpdate ->
            ExpressionRecordUpdate
                { recordVariable =
                    { range = expressionRecordUpdate.recordVariable.range
                    , value = expressionRecordUpdate.recordVariable.value
                    , type_ =
                        expressionRecordUpdate.recordVariable.type_
                            |> typeMapVariables typeVariableChange
                    }
                , fields =
                    expressionRecordUpdate.fields
                        |> List.map
                            (\field ->
                                { range = field.range
                                , name = field.name
                                , nameRange = field.nameRange
                                , value =
                                    field.value
                                        |> expressionTypedNodeMapTypeVariables
                                            typeVariableChange
                                }
                            )
                }

        ExpressionLambda expressionLambda ->
            ExpressionLambda
                { arguments =
                    expressionLambda.arguments
                        |> List.map
                            (\argument ->
                                argument |> patternTypedNodeMapTypeVariables typeVariableChange
                            )
                , result =
                    expressionLambda.result
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                }

        ExpressionCaseOf expressionCaseOf ->
            ExpressionCaseOf
                { matchedExpression =
                    expressionCaseOf.matchedExpression
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , cases =
                    expressionCaseOf.cases
                        |> List.map
                            (\case_ ->
                                { pattern =
                                    case_.pattern
                                        |> patternTypedNodeMapTypeVariables typeVariableChange
                                , result =
                                    case_.result
                                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                                }
                            )
                }

        ExpressionLetIn expressionLetIn ->
            ExpressionLetIn
                { declarations =
                    expressionLetIn.declarations
                        |> List.map
                            (\letDeclarationNode ->
                                letDeclarationNode
                                    |> Elm.Syntax.Node.map
                                        (\letDeclaration ->
                                            letDeclaration |> expressionLetDeclarationMapTypeVariables typeVariableChange
                                        )
                            )
                , result =
                    expressionLetIn.result
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                }


expressionLetDeclarationMapTypeVariables :
    (TypeVariableFromContext -> TypeVariableFromContext)
    -> LetDeclaration
    -> LetDeclaration
expressionLetDeclarationMapTypeVariables typeVariableChange expressionLetDeclaration =
    case expressionLetDeclaration of
        LetDestructuring letDestructuring ->
            LetDestructuring
                { pattern =
                    letDestructuring.pattern
                        |> patternTypedNodeMapTypeVariables typeVariableChange
                , expression =
                    letDestructuring.expression
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                }

        LetValueOrFunction letValueOrFunction ->
            LetValueOrFunction
                { signature = letValueOrFunction.signature
                , nameRange = letValueOrFunction.nameRange
                , name = letValueOrFunction.name
                , arguments =
                    letValueOrFunction.arguments
                        |> List.map
                            (\argument ->
                                argument |> patternTypedNodeMapTypeVariables typeVariableChange
                            )
                , result =
                    letValueOrFunction.result
                        |> expressionTypedNodeMapTypeVariables typeVariableChange
                , type_ =
                    letValueOrFunction.type_
                        |> typeMapVariables typeVariableChange
                }


patternTypedNodeSubstituteVariableByNotVariable :
    ModuleLevelDeclarationTypesInAvailableInModule
    -> { variable : TypeVariableFromContext, type_ : TypeNotVariable TypeVariableFromContext }
    -> TypedNode Pattern
    ->
        Result
            String
            { substitutions : VariableSubstitutions
            , node : TypedNode Pattern
            }
patternTypedNodeSubstituteVariableByNotVariable declarationTypes replacement patternTypedNode =
    -- IGNORE TCO
    case patternTypedNode.value of
        PatternUnit ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = patternTypedNode
                }

        PatternChar _ ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = patternTypedNode
                }

        PatternString _ ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = patternTypedNode
                }

        PatternInt _ ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = patternTypedNode
                }

        PatternIgnored ->
            Ok
                { substitutions = variableSubstitutionsNone
                , node = patternTypedNode
                }

        PatternVariable _ ->
            Result.map
                (\substituted ->
                    { substitutions = substituted.substitutions
                    , node =
                        { range = patternTypedNode.range
                        , value = patternTypedNode.value
                        , type_ = substituted.type_
                        }
                    }
                )
                (patternTypedNode.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        PatternParenthesized inParens ->
            Result.map
                (\inParensSubstituted ->
                    { substitutions = inParensSubstituted.substitutions
                    , node =
                        { range = patternTypedNode.range
                        , value = PatternParenthesized inParensSubstituted.node
                        , type_ = inParensSubstituted.node.type_
                        }
                    }
                )
                (inParens
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        PatternAs patternAs ->
            Result.map
                (\inParensSubstituted ->
                    { substitutions = inParensSubstituted.substitutions
                    , node =
                        { range = patternTypedNode.range
                        , value =
                            PatternAs
                                { pattern = inParensSubstituted.node
                                , variable =
                                    { range = patternAs.variable.range
                                    , value = patternAs.variable.value
                                    , type_ = inParensSubstituted.node.type_
                                    }
                                }
                        , type_ = inParensSubstituted.node.type_
                        }
                    }
                )
                (patternAs.pattern
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        PatternListCons patternListCons ->
            resultAndThen3
                (\headSubstituted tailSubstituted typeSubstituted ->
                    variableSubstitutionsMerge3 declarationTypes
                        headSubstituted.substitutions
                        tailSubstituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = patternTypedNode.range
                                    , value =
                                        PatternListCons
                                            { head = headSubstituted.node
                                            , tail = tailSubstituted.node
                                            }
                                    , type_ = typeSubstituted.type_
                                    }
                                }
                            )
                )
                (patternListCons.head
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (patternListCons.tail
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (patternTypedNode.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        PatternTuple patternTuple ->
            resultAndThen3
                (\part0Substituted part1Substituted typeSubstituted ->
                    variableSubstitutionsMerge3 declarationTypes
                        part0Substituted.substitutions
                        part1Substituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = patternTypedNode.range
                                    , value =
                                        PatternTuple
                                            { part0 = part0Substituted.node
                                            , part1 = part1Substituted.node
                                            }
                                    , type_ =
                                        -- TODO TypeTuple derive from part types
                                        typeSubstituted.type_
                                    }
                                }
                            )
                )
                (patternTuple.part0
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (patternTuple.part1
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (patternTypedNode.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        PatternTriple patternTriple ->
            resultAndThen4
                (\part0Substituted part1Substituted part2Substituted typeSubstituted ->
                    variableSubstitutionsMerge4 declarationTypes
                        part0Substituted.substitutions
                        part1Substituted.substitutions
                        part2Substituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = patternTypedNode.range
                                    , value =
                                        PatternTriple
                                            { part0 = part0Substituted.node
                                            , part1 = part1Substituted.node
                                            , part2 = part2Substituted.node
                                            }
                                    , type_ =
                                        -- TODO TypeTriple derive from part types
                                        typeSubstituted.type_
                                    }
                                }
                            )
                )
                (patternTriple.part0
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (patternTriple.part1
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (patternTriple.part2
                    |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )
                (patternTypedNode.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        PatternRecord patternRecordFields ->
            resultAndThen2
                (\fieldsSubstituted typeSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        fieldsSubstituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = patternTypedNode.range
                                    , value =
                                        PatternRecord
                                            (fieldsSubstituted.nodesReverse
                                                |> List.reverse
                                            )
                                    , type_ =
                                        -- TODO derive from field patterns
                                        typeSubstituted.type_
                                    }
                                }
                            )
                )
                (patternRecordFields
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\fieldNode soFar ->
                            fieldNode.type_
                                |> typeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\fieldTypeSubstituted ->
                                        variableSubstitutionsMerge declarationTypes
                                            fieldTypeSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\substitutionsWithField ->
                                                    { substitutions = substitutionsWithField
                                                    , nodesReverse =
                                                        { value = fieldNode.value
                                                        , range = fieldNode.range
                                                        , type_ = fieldTypeSubstituted.type_
                                                        }
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (patternTypedNode.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        PatternListExact patternListElement ->
            resultAndThen2
                (\elementsSubstituted typeSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        elementsSubstituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = patternTypedNode.range
                                    , value =
                                        PatternListExact
                                            (elementsSubstituted.nodesReverse
                                                |> List.reverse
                                            )
                                    , type_ = typeSubstituted.type_
                                    }
                                }
                            )
                )
                (patternListElement
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\elementNode soFar ->
                            elementNode
                                |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\fieldSubstituted ->
                                        variableSubstitutionsMerge declarationTypes
                                            fieldSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\substitutionsWithElement ->
                                                    { substitutions = substitutionsWithElement
                                                    , nodesReverse =
                                                        fieldSubstituted.node
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (patternTypedNode.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )

        PatternVariant patternVariant ->
            resultAndThen2
                (\elementsSubstituted typeSubstituted ->
                    variableSubstitutionsMerge declarationTypes
                        elementsSubstituted.substitutions
                        typeSubstituted.substitutions
                        |> Result.map
                            (\fullSubstitutions ->
                                { substitutions = fullSubstitutions
                                , node =
                                    { range = patternTypedNode.range
                                    , value =
                                        PatternVariant
                                            { qualification = patternVariant.qualification
                                            , name = patternVariant.name
                                            , moduleOrigin = patternVariant.moduleOrigin
                                            , arguments =
                                                elementsSubstituted.nodesReverse
                                                    |> List.reverse
                                            }
                                    , type_ = typeSubstituted.type_
                                    }
                                }
                            )
                )
                (patternVariant.arguments
                    |> listFoldlWhileOkFrom
                        { substitutions = variableSubstitutionsNone
                        , nodesReverse = []
                        }
                        (\argumentNode soFar ->
                            argumentNode
                                |> patternTypedNodeSubstituteVariableByNotVariable declarationTypes
                                    replacement
                                |> Result.andThen
                                    (\argumentSubstituted ->
                                        variableSubstitutionsMerge declarationTypes
                                            argumentSubstituted.substitutions
                                            soFar.substitutions
                                            |> Result.map
                                                (\substitutionsWithElement ->
                                                    { substitutions = substitutionsWithElement
                                                    , nodesReverse =
                                                        argumentSubstituted.node
                                                            :: soFar.nodesReverse
                                                    }
                                                )
                                    )
                        )
                )
                (patternTypedNode.type_
                    |> typeSubstituteVariableByNotVariable declarationTypes
                        replacement
                )


patternTypedNodeMapTypeVariables :
    (TypeVariableFromContext -> TypeVariableFromContext)
    -> TypedNode Pattern
    -> TypedNode Pattern
patternTypedNodeMapTypeVariables typeVariableChange patternTypedNode =
    { range = patternTypedNode.range
    , value =
        patternTypedNode.value
            |> patternMapTypeVariables typeVariableChange
    , type_ =
        patternTypedNode.type_
            |> typeMapVariables typeVariableChange
    }


patternMapTypeVariables :
    (TypeVariableFromContext -> TypeVariableFromContext)
    -> Pattern
    -> Pattern
patternMapTypeVariables typeVariableChange pattern =
    -- IGNORE TCO
    case pattern of
        PatternUnit ->
            PatternUnit

        PatternChar charValue ->
            PatternChar charValue

        PatternString stringValue ->
            PatternString stringValue

        PatternInt patternInt ->
            PatternInt patternInt

        PatternIgnored ->
            PatternIgnored

        PatternVariable variable ->
            PatternVariable variable

        PatternParenthesized inParens ->
            PatternParenthesized
                (inParens
                    |> patternTypedNodeMapTypeVariables typeVariableChange
                )

        PatternAs patternAs ->
            let
                patternWithTypeWithVariablesChanged : TypedNode Pattern
                patternWithTypeWithVariablesChanged =
                    patternAs.pattern
                        |> patternTypedNodeMapTypeVariables typeVariableChange
            in
            PatternAs
                { pattern = patternWithTypeWithVariablesChanged
                , variable =
                    { range = patternAs.variable.range
                    , value = patternAs.variable.value
                    , type_ = patternWithTypeWithVariablesChanged.type_
                    }
                }

        PatternListCons patternListCons ->
            PatternListCons
                { head =
                    patternListCons.head
                        |> patternTypedNodeMapTypeVariables typeVariableChange
                , tail =
                    patternListCons.tail
                        |> patternTypedNodeMapTypeVariables typeVariableChange
                }

        PatternTuple patternTuple ->
            PatternTuple
                { part0 =
                    patternTuple.part0
                        |> patternTypedNodeMapTypeVariables typeVariableChange
                , part1 =
                    patternTuple.part1
                        |> patternTypedNodeMapTypeVariables typeVariableChange
                }

        PatternTriple patternTriple ->
            PatternTriple
                { part0 =
                    patternTriple.part0
                        |> patternTypedNodeMapTypeVariables typeVariableChange
                , part1 =
                    patternTriple.part1
                        |> patternTypedNodeMapTypeVariables typeVariableChange
                , part2 =
                    patternTriple.part2
                        |> patternTypedNodeMapTypeVariables typeVariableChange
                }

        PatternRecord patternRecordFields ->
            PatternRecord
                (patternRecordFields
                    |> List.map
                        (\field ->
                            { value = field.value
                            , range = field.range
                            , type_ =
                                field.type_
                                    |> typeMapVariables typeVariableChange
                            }
                        )
                )

        PatternListExact patternListElement ->
            PatternListExact
                (patternListElement
                    |> List.map
                        (\element ->
                            element
                                |> patternTypedNodeMapTypeVariables
                                    typeVariableChange
                        )
                )

        PatternVariant patternVariant ->
            PatternVariant
                { qualification = patternVariant.qualification
                , name = patternVariant.name
                , moduleOrigin = patternVariant.moduleOrigin
                , arguments =
                    patternVariant.arguments
                        |> List.map
                            (\argument ->
                                argument
                                    |> patternTypedNodeMapTypeVariables
                                        typeVariableChange
                            )
                }


equivalentVariablesCreateCondensedVariable : FastSet.Set TypeVariableFromContext -> Result String TypeVariableFromContext
equivalentVariablesCreateCondensedVariable set =
    case set |> FastSet.toList of
        [] ->
            Err "implementation bug: equivalent variables set is empty"

        headVariable :: tailVariables ->
            tailVariables
                |> listFoldlWhileOkFrom
                    (headVariable |> typeVariableFromContextName |> typeVariableConstraint)
                    (\variable soFar ->
                        maybeTypeVariableConstraintMerge
                            (variable |> typeVariableFromContextName |> typeVariableConstraint)
                            soFar
                    )
                |> Result.map
                    (\unifiedConstraint ->
                        ( -- creating a new variable safely
                          (headVariable :: tailVariables)
                            |> List.map
                                (\( context, name ) ->
                                    name :: context
                                )
                            |> List.sort
                            |> List.intersperse [ "_and" ]
                            |> List.concat
                        , "equivalent"
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


fastDictMapAndSmallestJust : (key -> value -> Maybe ok) -> FastDict.Dict key value -> Maybe ok
fastDictMapAndSmallestJust keyValueToMaybe fastDict =
    fastDict
        |> FastDict.stoppableFoldl
            (\key value _ ->
                case keyValueToMaybe key value of
                    Just foldedWithEntry ->
                        Just foldedWithEntry |> FastDict.Stop

                    Nothing ->
                        Nothing |> FastDict.Continue
            )
            Nothing


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
    -> { errors : List String, types : ModuleTypes }
moduleInterfaceToTypes moduleInterface =
    let
        typeAliases :
            { errors : List String
            , types :
                FastDict.Dict
                    String
                    { type_ : Type String, parameters : List String }
            }
        typeAliases =
            moduleInterface.aliases
                |> List.foldl
                    (\typeAliasDeclarationInterface soFar ->
                        case
                            typeAliasDeclarationInterface.tipe
                                |> interfaceToType
                        of
                            Err error ->
                                { errors = error :: soFar.errors
                                , types = soFar.types
                                }

                            Ok type_ ->
                                { errors = soFar.errors
                                , types =
                                    soFar.types
                                        |> FastDict.insert
                                            typeAliasDeclarationInterface.name
                                            { type_ = type_
                                            , parameters = typeAliasDeclarationInterface.args
                                            }
                                }
                    )
                    { types = FastDict.empty
                    , errors = []
                    }

        choiceTypes :
            { errors : List String
            , types :
                FastDict.Dict
                    String
                    { parameters : List String
                    , variants : FastDict.Dict String (List (Type String))
                    }
            }
        choiceTypes =
            moduleInterface.unions
                |> List.foldl
                    (\declarationChoiceType soFar ->
                        case
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
                        of
                            Err error ->
                                { errors = error :: soFar.errors
                                , types = soFar.types
                                }

                            Ok variants ->
                                { errors = soFar.errors
                                , types =
                                    soFar.types
                                        |> FastDict.insert
                                            declarationChoiceType.name
                                            { parameters =
                                                declarationChoiceType.args
                                            , variants = variants
                                            }
                                }
                    )
                    { types = FastDict.empty
                    , errors = []
                    }

        signatures : { errors : List String, types : FastDict.Dict String (Type String) }
        signatures =
            moduleInterface.values
                |> List.foldl
                    (\valueOrFunctionDeclarationInterface soFar ->
                        case
                            valueOrFunctionDeclarationInterface.tipe
                                |> interfaceToType
                        of
                            Err error ->
                                { errors = error :: soFar.errors
                                , types = soFar.types
                                }

                            Ok type_ ->
                                { errors = soFar.errors
                                , types =
                                    soFar.types
                                        |> FastDict.insert
                                            valueOrFunctionDeclarationInterface.name
                                            type_
                                }
                    )
                    { types = FastDict.empty
                    , errors = []
                    }
    in
    { errors =
        typeAliases.errors
            ++ choiceTypes.errors
            ++ signatures.errors
    , types =
        { typeAliases = typeAliases.types
        , choiceTypes = choiceTypes.types
        , signatures = signatures.types
        }
    }


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
    -> { types : ModuleTypes, errors : List String }
moduleDeclarationsToTypes moduleOriginLookup declarations =
    declarations
        |> List.foldl
            (\declaration soFar ->
                case declaration of
                    Elm.Syntax.Declaration.InfixDeclaration _ ->
                        soFar

                    Elm.Syntax.Declaration.Destructuring _ _ ->
                        { errors =
                            "destructuring at the module level is invalid syntax"
                                :: soFar.errors
                        , types = soFar.types
                        }

                    Elm.Syntax.Declaration.FunctionDeclaration declarationValueOrFunction ->
                        case declarationValueOrFunction.signature of
                            Nothing ->
                                soFar

                            Just (Elm.Syntax.Node.Node _ declarationValueOrFunctionSignature) ->
                                case
                                    declarationValueOrFunctionSignature.typeAnnotation
                                        |> Elm.Syntax.Node.value
                                        |> syntaxToType moduleOriginLookup
                                of
                                    Err error ->
                                        { errors = error :: soFar.errors
                                        , types = soFar.types
                                        }

                                    Ok type_ ->
                                        { errors = soFar.errors
                                        , types =
                                            { signatures =
                                                soFar.types.signatures
                                                    |> FastDict.insert
                                                        (declarationValueOrFunctionSignature.name |> Elm.Syntax.Node.value)
                                                        type_
                                            , typeAliases = soFar.types.typeAliases
                                            , choiceTypes = soFar.types.choiceTypes
                                            }
                                        }

                    Elm.Syntax.Declaration.AliasDeclaration declarationTypeAlias ->
                        case
                            declarationTypeAlias.typeAnnotation
                                |> Elm.Syntax.Node.value
                                |> syntaxToType moduleOriginLookup
                        of
                            Err error ->
                                { errors = error :: soFar.errors
                                , types = soFar.types
                                }

                            Ok type_ ->
                                { errors = soFar.errors
                                , types =
                                    { signatures = soFar.types.signatures
                                    , typeAliases =
                                        soFar.types.typeAliases
                                            |> FastDict.insert
                                                (declarationTypeAlias.name |> Elm.Syntax.Node.value)
                                                { parameters =
                                                    declarationTypeAlias.generics
                                                        |> List.map Elm.Syntax.Node.value
                                                , type_ = type_
                                                }
                                    , choiceTypes = soFar.types.choiceTypes
                                    }
                                }

                    Elm.Syntax.Declaration.CustomTypeDeclaration declarationChoiceType ->
                        case
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
                        of
                            Err error ->
                                { errors = error :: soFar.errors
                                , types = soFar.types
                                }

                            Ok variants ->
                                { errors = soFar.errors
                                , types =
                                    { signatures = soFar.types.signatures
                                    , typeAliases = soFar.types.typeAliases
                                    , choiceTypes =
                                        soFar.types.choiceTypes
                                            |> FastDict.insert
                                                (declarationChoiceType.name |> Elm.Syntax.Node.value)
                                                { parameters =
                                                    declarationChoiceType.generics
                                                        |> List.map Elm.Syntax.Node.value
                                                , variants = variants
                                                }
                                    }
                                }

                    Elm.Syntax.Declaration.PortDeclaration declarationPortSignature ->
                        case
                            declarationPortSignature.typeAnnotation
                                |> Elm.Syntax.Node.value
                                |> syntaxToType moduleOriginLookup
                        of
                            Err error ->
                                { errors = error :: soFar.errors
                                , types = soFar.types
                                }

                            Ok type_ ->
                                { errors = soFar.errors
                                , types =
                                    { signatures =
                                        soFar.types.signatures
                                            |> FastDict.insert
                                                (declarationPortSignature.name |> Elm.Syntax.Node.value)
                                                type_
                                    , typeAliases = soFar.types.typeAliases
                                    , choiceTypes = soFar.types.choiceTypes
                                    }
                                }
            )
            { types =
                { signatures = FastDict.empty
                , typeAliases = FastDict.empty
                , choiceTypes = FastDict.empty
                }
            , errors = []
            }


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


resultAndThen2 :
    (a -> b -> Result error c)
    -> Result error a
    -> Result error b
    -> Result error c
resultAndThen2 abToResult aResult bResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    abToResult a b


resultAndThen3 :
    (a -> b -> c -> Result error d)
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
resultAndThen3 abToResult aResult bResult cResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    case cResult of
                        Err error ->
                            Err error

                        Ok c ->
                            abToResult a b c


resultAndThen4 :
    (a -> b -> c -> d -> Result error e)
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
    -> Result error e
resultAndThen4 abToResult aResult bResult cResult dResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    case cResult of
                        Err error ->
                            Err error

                        Ok c ->
                            case dResult of
                                Err error ->
                                    Err error

                                Ok d ->
                                    abToResult a b c d


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
