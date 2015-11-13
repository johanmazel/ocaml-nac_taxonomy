
open Printf

type t =
| Equal
| Different
with compare, sexp, bin_io

let of_string
    string
  =
  match string with
  | "=" -> Equal
  | "<>" -> Different
  | _ -> 
    (
      print_endline
        (sprintf 
           "Comparison_criteria: of_string: invalid string: %s" 
           string
        );
      assert(false);
    )

let to_string
    t
    =
  match t with
  | Equal -> "="
  | Different -> "<>"
