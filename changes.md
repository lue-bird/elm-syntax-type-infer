## 2.0.0 (unreleased)
considering
- rename `matchedExpression` to `matched`
- embed origin module if from local module top level as the actual name, not []
- split ExpressionReference into ReferenceLetDeclaration, ReferencePatternVariable and ReferenceModuleDeclaration(, ReferenceVariant, ReferenceRecordTypeAliasConstructor)

optimization ideas
- optimize typeListUnify. unifying long lists of uses takes ages.
  Prefer either ane-after-another unification in existing traversal.
  Especially useful for things like substitutionsForUnifyingIntroducedVariableTypesWithUsesInExpression!
- for annotated let and top-level declarations, unify parameters and annotation types _before_
- special-case declarations without parameters
- go through typeUnify and add e.g. typeUnifyWithFunction
- try when collecting uses not from FastDict.Dict String _ but regular List { String }
  since introduced variables are usually few
- make ropeFoldlWhileOkFrom TCO
- (breaking) lookup by qualification ++ "." ++ name, List String tuple might be slow to compare
- convert Result to { ok, List error }

#### 1.0.10 (unreleased)
- update top-level unannotated declaration instances across more than one other declaration
- faster (TODO in real world situation it was actually only equal, investigate!)

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
