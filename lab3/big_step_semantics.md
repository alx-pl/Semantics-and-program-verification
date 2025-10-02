In the case of *Big Steps* semantics, a non-terminal configuration is supposed to perform a single step directly to a final configuration.

Today's exercises are meant to be done on whiteboard.


1. Write a Big Steps semantics for Expr from Tiny. Note the necessary rules of the form *(n, s) → n*, which evaluate numbers to final configurations.
2. Try to add error handling, like `"division by 0"` or `"unknown variable"`. Make sure that the semantics is deterministic (e.g. *(1 / 0) + x*, where *x* is not defined). Discuss symmetry breaking when enforcing the execution order on subterms.
3. Propose a *Big Steps* semantics for *Tiny*. Discuss the rule for while – try to make it compositional by using only sub-programs of the given program – do not rely on if-then-else.
4. Compare it to small-steps semantics in terms of involved proof sizes – in small step semantics the proof sizes can be bounded by a function of the size of the program, while in big step semantics they depend also on initial state.
5. Discuss how to deal with break-continue, what is the life-cycle of the added flags?
6. Go back to Expr and add the construction `"let...in..."`.
7. What should we change if `"let...in..."` was meant to be lazy? Remember the expression which has to be computed.
8. Discuss inductive construction of nested State and the existence of general Scott domains.
9. Notice that together with the expression, one has to remember the state in which it should be evaluated (give an example!).
10. Add the constructs *(λ x . e)* and *(e<sub>>1</sub> (e<sub>2</sub>))* to *Expr*. Give an example of a correct expression where *e<sub>1</sub>* in application is NOT of the form *(λ x . e)*.
11. Discuss their intended semantics. How to represent functions in configurations? (we take syntactic approach, not functional), so *Fun = Var x Expr x State*.
12. Write down the full big steps semantics of Expr with lambda abstaction and application.
13. Again ask what happens if we want to have lazy evaluation.
14. Express `"let...in..."` in terms of the other constructs.
