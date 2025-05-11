#### 1.0.7 (unreleased)
TODO
- add new let type instances to introducedTypeVariables
  or switch to a different localized substitution model (e.g. always treat expression variables / lets from higher up as generic and replace their types only at that higher level - combined with - not propagating substitutions of outer references up, instead from the top finding contained references and unifying all their types with what's already known)
- do _not_ instantiate let type variables that are also present higher up
  (e.g. in outer declaration top level annotation)

considering
- embed origin module if from local module top level as the actual name, not []
- split ExpressionReference into ReferenceLetDeclaration, ReferencePatternVariable and ReferenceModuleDeclaration(, ReferenceVariant, ReferenceRecordTypeAliasConstructor)

optimization ideas
- special-case declarations without parameters
- go through typeUnify and add e.g. typeUnifyWithFunction
- optimize for the case that all types do match (e.g. check for type equivalence, then shortcut)

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
