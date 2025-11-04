open Oddx

let () =
  let x = init 10. in
  let y = init 5. in
  let z = init 15. in
  assert (get_val x = 10.);
  assert ((x +! y) =! z);
  assert ((x *! y) =! init 50.);
  
  let x = init 10. in
  let z = x +! x in
  backward z;
  assert (get_grad x = 2.);
  
  let x = init 10. in
  let z = x *! x in
  backward z;
  assert(get_grad x = 20.);


  let a = init (-1.) in
  let b = init 3. in
  let z = a *! (b +! a) in
  backward z;
  assert(get_grad a = 1.);
  assert(get_grad b = -1.);


  (* -------------- Minus ----------------*)
  let a = init 1. in
  let b = init 2. in
  let f = (a -! b) *! (a +! b) in
  backward f;
  assert(get_grad a = 2.);
  assert(get_grad b = -4.);


  (* --------------- Fractions -----------*)
  let a = init 2. in
  let b = init (-3.) in
  let f = a /! b in
  backward f;
  assert(get_grad a = 1. /. (-3.));
  assert(get_grad b = -2. /. (9.));
  

  let a = init 2. in
  let b = init (-3.) in
  let f = (a +! b) /! (a *! a *! b) in
  (* dfda = (a^2 b - (a + b)(2ab))/(a^4b^2) -> dfda(2, -3) = (-12 + -12) / (16 * 9)*)
  (* dfdb = (a^2 b - (a + b)(a^2))/(a^4b^2) -> dfdb(2, -3) = (-12 + 4) / (16 * 9) *)
  backward f;
  assert(get_grad a = -24. /. (16. *. 9.));
  assert(get_grad b = -8. /. (16. *. 9.));
  
  backward f;
  assert(get_grad a = -24. /. (16. *. 9.));
  assert(get_grad b = -8. /. (16. *. 9.));


  (* -----------------------functions ----------------- *)
  let x = init 1. in
  let f = oddx_exp x in
  backward f;
  assert(get_grad x = Float.exp(1.));
  let x = init 2. in
  let y = init (-1.) in
  let f = oddx_exp @@ x *! y in
  backward f;
  assert(get_grad x = -1. *. Float.exp(-2.));
  assert(get_grad y = 2. *. Float.exp(-2.));

  let x = init 3.5 in
  let f = init 1. /! (init 1. +! oddx_exp(init 0. -! x) ) in
  backward f;
  assert(get_grad x = 1. /. (Float.exp 3.5 *. (1. +. Float.exp(-3.5)) ** 2.0));
  
  
  (* -------------------- Power operator ----------------- *)
  let a = init 2. in 
  let b = init (-3.) in
  let f = a ^! b in
  backward f;
  assert(get_grad a = -3. *. 2. ** (-4.));
  assert(get_grad b = Float.log(2.) *. 2. ** (-3.));

  
  Printf.printf "All tests passed!\n";