# LogicParser
A parser for propositional and possibly predicate logic in haskell.

# Requirements
You need have installed GHCi

# Instructions
- Start GHCi on your terminal.
- Load LogicInterp.hs

The only function working at the moment is showExpr.

There are two atoms already defined in LogicInterp.hs, namely p and q.
- p = EVar (Ident "p")
- q = EVar (Ident "q")

You can use these right off the bat, but if you want to use more variables,
you'll have to write them out manually, like above.

After loading the LogicInterp.hs file in GHCi, try this:
- (EAnd (ENot p) q)

# UPDATE:
Functions:

```haskell
asValue :: Map.Map Ident Bool -> Expr -> Bool -> Map.Map Ident Bool
sEval :: Map.Map Ident Bool -> Expr -> Bool
```

asValue can now assign a value to a variable, EVar (Ident "s").
Start off with an empty map in your terminal

```haskell
let x =  asValue Map.empty (Ident "s") Bool.
```
If you want add more then use x for the map, but another "let" variable:
```haskell
let m = asValue x (Ident "r") Bool.
```

After you've inserted values for your variables with asValue, try constructing
a simple expression existing of basic proporsitional logic (EAnd, EOr, EImp, ENot).

