
type t = {
  v : float;
  g : float;
  op : op option;
}
and 
op = 
  | Add of t * t
  | Mul of t * t


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

let ( *! ) x y = 
  {
    v = x.v *. y.v;
    g = 0.;
    op = Some (Mul (x, y)) 
  }

let string_history x = 
  match x.op with
  | None -> "No history"
  | Some _ -> "history"

