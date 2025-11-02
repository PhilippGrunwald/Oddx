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

  assert(true);

  Printf.printf "All tests passed!\n";