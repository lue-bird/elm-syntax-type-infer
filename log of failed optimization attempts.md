- monomorphize Type, Expression, Pattern (no to negative effect)
- optimize typeVariablesFromContextToDisambiguationLookup by adding a regular set for tracking disambiguated variables
  (no change in performance)
- expand un-annotated (let) declarations early before substituting everything else
  (or checking if shallowly inferred un-annotated let declaration does not contain type variables (or is potentially even just maximally concrete: all type variables' use ranges extend beyond the let declaration))
- skip substitutions when range does not include variable-to-replace' use range
  (was way too slow and ineffective, as most substitutions were already local or covered a wide range)
- always use the unified type when available because it can potentially avoid duplicate  work and allocation
- change `DictByTypeVariableFromContext value`'s internal representation to `DictByRange (Dict String value)` (roughly equal)
- change `DictByTypeVariableFromContext value`'s internal representation to `DictByRange (List { name : String, value : value })` (about 50% slower)
- make ropeFoldlWhileOkFrom TCO (pretty much equal, just maybe a tiny bit slower)
- specialized Set instead of DictBy... () to avoid extra memory for unit fields (no detectible performance change but less code reuse)
- for annotated let and top-level declarations, unify parameters and annotation types _before_ (somehow this was slower but I couldn't tell you why. It was also more error prone)
