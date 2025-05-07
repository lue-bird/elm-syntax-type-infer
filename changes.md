#### 1.0.3 optimization ideas
- go through typeUnify and add e.g. typeUnifyWithFunction
- idea: optimize for the case that all types do match (e.g. check for type equivalence, then shortcut)

#### 1.0.2
- correctly infer type of an operator in an infix operation

#### 1.0.1
- correctly infer usage of destructured pattern variables across the whole let-in
