#### 2.0.1 (unreleased)
TODO
- do _not_ instantiate let type variables that are also present higher up
  (e.g. in outer declaration top level annotation)
    - how?
      Each TypeVariableFromContext now also stores
      it's origin Range.
      Wether a type variable is forall is then entirely determined by
      if the let declaration Range includes the type variable Range.

      On condensing, make sure to Range.combine across all variables.

      Add a new field to expressionTypeInfer context:
          `reusedTypeVariablesAcrossAnnotations : FastDict.Dict String Range`
      collected in a separate step where all Ranges by type var name are combined
      across the whole top-level declaration

      (to verify) Side benefit: all path checking and storing can be removed as the range is already unique (→ no more FromEnd ++ index, context altering etc!)

considering
- embed origin module if from local module top level as the actual name, not []
- split ExpressionReference into ReferenceLetDeclaration, ReferencePatternVariable and ReferenceModuleDeclaration(, ReferenceVariant, ReferenceRecordTypeAliasConstructor)

optimization ideas
- special-case declarations without parameters
- go through typeUnify and add e.g. typeUnifyWithFunction


## 2.0.0
- types now parameterize type variables, not types

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
