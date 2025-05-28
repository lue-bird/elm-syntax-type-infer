## 2.0.0 (unreleased)
considering
- rename `matchedExpression` to `matched`
- embed origin module if from local module top level as the actual name, not []
- split ExpressionReference into ReferenceLetDeclaration, ReferencePatternVariable and ReferenceModuleDeclaration(, ReferenceVariant, ReferenceRecordTypeAliasConstructor)

optimization ideas
- optimize typeVariablesFromContextToDisambiguationLookup by adding a regular set for tracking disambiguated variables
- represent List EquivalentVariableSet as FastDict.Dict RangeAsComparable info
  to speed up merge with 2
- check if we can only substitute patterns when applying substitutions from unifying with uses, and leave the result pattern as is
- special-case declarations without parameters
- go through typeUnify and add e.g. typeUnifyWithFunction
- lookup by qualification ++ "." ++ name, List String tuple might be slow to compare
- represent LocationAsComparable as row + startColumn * 2^24 to save memory

#### 1.0.10 (unreleased)
- update top-level unannotated declaration instances across more than one other declaration
- faster (TODO in real world situation it was actually 2x slower, investigate!)

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
