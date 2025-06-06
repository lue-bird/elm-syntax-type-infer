## 2.0.0 (unreleased)
- do not parameterize type variables and types, just use regular Type for everything
- represent module name as plain string instead of `ELm.Syntax.ModuleName.ModuleName`
- split `ExpressionReference` into `ExpressionReferenceVariant`, `ExpressionReferenceRecordTypeAliasConstructorFunction` and `ExpressionReference`
- in variant `ExpressionCaseOf`, rename `matchedExpression` to `matched`
- in variant `PatternVariant`, add field `choiceTypeName`
- `valueAndFunctionDeclarations` and `moduleDeclarationsToTypes` now additionally require the module name. In return, references to things declared in the current module will have the current module name as their origin
- `moduleDeclarationsToTypes` now accepts declaration nodes instead of just declarations
  This is usually more convenient and can avoid a List.map Node.value
- keep use ranges at resulting type variables for the output
  to determine which variables are introduced by a let
- a bit faster

optimization ideas
- for annotated let and top-level declarations, unify parameters and annotation types _before_
- special-case declarations without parameters
- go through typeUnify and add e.g. typeUnifyWithFunction
- specialized Set instead of DictBy... () to avoid extra memory for unit fields
- typeVariablesFromContextToDisambiguationLookup use DictByTypeVariableFromContext.mapAccum

#### 1.0.10
- update top-level unannotated declaration instances across more than one other declaration
- correct very rare edge cases where type variables that should be the same were independent

#### 1.0.9
- correctly infer imported record type alias constructor

#### 1.0.8
- correctly don't instantiate let type variables that are also used outside the let declaration
- faster

#### 1.0.7
- internal: significantly disentangle logic, now more bottom-up inferring and top-down unifying.
  This will likely also be faster

#### 1.0.6
- correct endless recursion with let value being used more strictly (introduced in 1.0.4)

#### 1.0.5
- correct oversights from 1.0.4

#### 1.0.4
- correctly infer un-annotated let value/function declarations

#### 1.0.3
- correctly infer when an inner type variable that's equivalent to an outer type variable is substituted to a not-fully-concrete non-variable type

#### 1.0.2
- correctly infer type of an operator in an infix operation

#### 1.0.1
- correctly infer usage of destructured pattern variables across the whole let-in
