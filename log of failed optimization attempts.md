- monomorphize Type, Expression, Pattern (no to negative effect)
- optimize typeVariablesFromContextToDisambiguationLookup by adding a regular set for tracking disambiguated variables
  (no change in performance)
- expand un-annotated (let) declarations early before substituting everything else
  (or checking if shallowly inferred un-annotated let declaration does not contain type variables (or is potentially even just maximally concrete: all type variables' use ranges extend beyond the let declaration))
- skip substitutions when range does not include variable-to-replace' use range
  (was way too slow and ineffective, as most substitutions were already local or covered a wide range)
- always use the unified type when available because it can potentially avoid duplicate  work and allocation
