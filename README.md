# LogicParser
A parser for propositional and possibly predicate logic in haskell.

# Requirements
You need have installed GHCi

# Instruction
- Start GHCi on your terminal.
- Load LogicInterp.hs
       The only function working at the moment is showExpr.

# How to use
There are two atoms already defined in LogicInterp.hs, namely p and q.
      p = EVar (Ident "p")
      q = EVar (Ident "q")
You can use these right off the bat, but if you want to use more variables,
you'll have to write them out manually, like above.

After loading the LogicInterp.hs file in GHCi, try this:
(EAnd (ENot p) q)