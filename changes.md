#### 2.0.7 (unreleased)
TODO
- try avoid creating callResult when possible because it leads to bad error messages.
  Instead unroll called type and unify parameter types then + remaining type is the result type

#### 2.0.6
- correct List, tuple and triple parts not being inferred as compappend/comparable when type variables are appendable/have no constraint and the whole thing is unified with a comparable. In these cases, there should be a substitution created with the combined constraint
- correct `String.String` not being seen as `compappend`

#### 2.0.5
- correct edge case where using a record type variable in multiple places could prevent it from being inferred to a more concrete type

#### 2.0.4
- correct `Char.Char` not being regarded as comparable

#### 2.0.3
- correct edge case where inexhaustive record pattern type was incorrectly inferred as a record instead of record extension

#### 2.0.2
- correct edge case where third part in a triple was incorrectly set to the second part

#### 2.0.1
- correct how the module origin is looked up when an import alias and alias/module name of other imports overlap

## 2.0.0
- do not parameterize type variables and types, just use regular Type for everything
- represent module name as plain string instead of `ELm.Syntax.ModuleName.ModuleName`
- split `ExpressionReference` into `ExpressionReferenceVariant`, `ExpressionReferenceRecordTypeAliasConstructorFunction` and `ExpressionReference`
- in variant `ExpressionCaseOf`, rename `matchedExpression` to `matched`
- in variant `PatternVariant`, add field `choiceTypeName`
- `valueAndFunctionDeclarations` and `moduleDeclarationsToTypes` now additionally require the module name. In return, references to things declared in the current module will have the current module name as their origin
- `valueAndFunctionDeclarations` now returns a list with the declaration name included instead of a dict by name
- `moduleDeclarationsToTypes` now accepts declaration nodes instead of just declarations
  This is usually more convenient and can avoid a List.map Node.value
- keep use ranges at resulting type variables for the output
  to determine which variables are introduced by a let
- correct very rare edge cases where type variables that should be the same were independent
- faster

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
