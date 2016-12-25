
open Printf

type t =
| Greater
| Greater_or_equal
| Lower
| Lower_or_equal
[@@deriving compare, sexp, bin_io]

let of_string
    string
  =
  match string with
  | ">" -> Greater
  | ">=" -> Greater_or_equal
  | "<" -> Lower
  | "<=" -> Lower_or_equal
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
  | Greater -> ">"
  | Greater_or_equal -> ">="
  | Lower -> "<"
  | Lower_or_equal -> "<="
