type oddx_fun = 
  | Oddx_Exp
  | Oddx_Sin
  | Oddx_Cos



type t = {
  v : float;
  mutable g : float;
  op : op option;
}
and 
op = 
  | Add of t * t
  | Minus of t * t
  | Mul of t * t
  | Frac of t * t
  | OddxFun of oddx_fun * t



(* ------------------------------ utils ------------------------------- *)

let init f = 
  {
    v = f;
    g = 0.;
    op = None
  }

let get_val x = x.v

let get_grad x = x.g

let string_history x = 
  match x.op with
  | None -> "No history"
  | Some _ -> "history"



(* ---------------------------- operators ------------------------- *)
let ( +! ) x y = 
  {
    v = x.v +. y.v;
    g = 0.;
    op = Some(Add (x,y))
  }

let ( -! ) x y = 
  {
    v = x.v -. y.v;
    g = 0.;
    op = Some (Minus (x, y))
  }

let ( *! ) x y = 
  {
    v = x.v *. y.v;
    g = 0.;
    op = Some (Mul (x, y)) 
  }


let ( /! ) x y = 
  {
    v = x.v /. y.v;
    g = 0.;
    op = Some (Frac (x, y))
  } 

let ( =! ) x y = (x.v = y.v)



(* ----------------------------- functions -------------------------------- *)

let calc_oddx_fun oddx_function x = 
  match oddx_function with
  | Oddx_Exp -> Float.exp x
  | Oddx_Sin -> Float.sin x
  | Oddx_Cos -> Float.cos x

let calc_oddx_fun_der oddx_function x = 
  match oddx_function with
  | Oddx_Exp -> Float.exp x
  | Oddx_Sin -> Float.cos x
  | Oddx_Cos -> -1. *. Float.sin x

let generic_user_fun oddx_function a = 
  {
    v = calc_oddx_fun oddx_function a.v;
    g = 0.;
    op = Some (OddxFun (oddx_function, a))
  }

let oddx_exp a = generic_user_fun Oddx_Exp a
let oddx_sin a = generic_user_fun Oddx_Sin a
let oddx_cos a = generic_user_fun Oddx_Cos a


let rec flush node = 
  match node.op with
  | None -> node.g <- 0.;
  | Some formula ->
    match formula with
    | Add(a, b) | Mul (a, b) | Minus (a, b) | Frac (a, b) -> begin
      node.g <- 0.;
      flush a; flush b
      end
    | OddxFun (_, a) -> flush a


let backward z =
  let rec aux node = 
    match node.op with 
    | None -> ();
    | Some f -> begin
      match f with
      | Add (a, b) -> begin
        a.g <- a.g +. node.g;
        b.g <- b.g +. node.g;
        aux a; aux b; 
        end
      | Mul (a, b) -> begin
        a.g <- a.g +. b.v *. node.g;  
        b.g <- b.g +. a.v *. node.g;
        aux a; aux b;
        end
      | Minus (a, b) -> begin
        a.g <- a.g +. node.g;
        b.g <- b.g -. node.g;
        aux a; aux b;
        end
      | Frac (a, b) -> begin
        a.g <- a.g +. node.g /. b.v;
        b.g <- b.g -. a.v /. (b.v *. b.v) *. node.g;
        aux a; aux b;
        end
      | OddxFun (f, a) -> begin
        a.g <- a.g +. (calc_oddx_fun_der f a.v) *. node.g;
        aux a;
        end
      end
  in
  flush z;
  z.g <- 1.0;
  aux z
