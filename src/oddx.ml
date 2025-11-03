
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



let init f = 
  {
    v = f;
    g = 0.;
    op = None
  }

let get_val x = x.v

let get_grad x = x.g

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

(* 
f = y * (x + y) 

-> f = y * (g) with g = x + y
-> df/dy = 1 * (g) + y * dg/dy 
*)

let backward z =
  let rec aux node = 
    match node.op with 
    | None -> begin end
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
      end
  in
  z.g <- 1.0;
  aux z
  

let string_history x = 
  match x.op with
  | None -> "No history"
  | Some _ -> "history"

