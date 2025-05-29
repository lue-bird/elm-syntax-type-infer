- monomorphize Type, Expression, Pattern (no to negative effect)
- optimize typeVariablesFromContextToDisambiguationLookup by adding a regular set for tracking disambiguated variables
- expand un-annotated (let) declarations early before substituting everything else
  (or checking if shallowly inferred un-annotated let declaration does not contain type variables (or is potentially even just maximally concrete: all type variables' use ranges extend beyond the let declaration))
