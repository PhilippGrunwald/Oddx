open Oddx

let () =
  let x = init 10. in
  assert (get_val x = 10.);
  Printf.printf "All tests passed!\n"