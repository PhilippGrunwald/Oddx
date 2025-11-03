## Oddx
*Oddx* is an [automatic differentiation (AD)](https://en.wikipedia.org/wiki/Automatic_differentiation) engine for OCaml. Simply initialise the variables that you want to use
```ocaml
open oddx

let x = init 10. in
let y = init 20. in
```
and use the oddx-operators (all with an ! at the end) to do your calculations
```ocaml
let f = (a +! b) /! (a *! a *! b) in
```
and obtain (only calculated on demand via backwards mode AD) the partial derivatives of f with respect to a and b at the position $(10, 20)$ via
```ocaml
backward f;
(* dfda = (a^2 b - (a + b)(2ab))/(a^4b^2) -> dfda(2, -3) = (-12 + -12) / (16 * 9)*)
(* dfdb = (a^2 b - (a + b)(a^2))/(a^4b^2) -> dfdb(2, -3) = (-12 + 4) / (16 * 9) *)
assert(get_grad a = -24. /. (16. *. 9.));
assert(get_grad b = -8. /. (16. *. 9.));
```
