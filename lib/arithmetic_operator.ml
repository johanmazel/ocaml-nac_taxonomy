
open Printf

type t =
| Plus
| Minus
| Time
| Divide
with compare, sexp, bin_io

let of_string
    string
    =
  match string with
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Time
  | "/" -> Divide
  | _ -> 
    (
      print_endline
  (sprintf 
     "Arithmetic_operator: of_string: invalid string: %s" 
     string
  );
      assert(false);
    )

let to_string
    t
    =
  match t with
  | Plus -> "+"
  | Minus -> "-"
  | Time -> "*"
  | Divide -> "/"

let apply t1 t2 t =
  match t with
  | Plus -> t1 +. t2
  | Minus -> t1 -. t2
  | Time -> t1 *. t2
  | Divide -> 
    if t2 <> 0.0 then 
      t1 /. t2
    else
      0.
