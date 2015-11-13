
type t =
| AND
| OR
with compare, sexp, bin_io

let apply t bool_1 bool_2 =
  match t with
  | AND -> bool_1 && bool_2
  | OR -> bool_1 || bool_2

let of_string
    string
    =
  match string with
  (* | "AND" -> AND *)
  (* | "OR" -> OR *)
  | "&&" -> AND
  | "||" -> OR
  | _ -> assert(false)
