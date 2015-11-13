
open Printf

type t =
| Category
| Standard
| Default
with compare, sexp, bin_io

let of_string string =
  match string with
  | "C" -> Category
  | "S" -> Standard
  | "D" -> Default
  | _ -> failwith (sprintf "Anomaly_signature_type: of_string: invalid string: %s" string)

let to_string t =
  match t with
  | Category -> "C"
  | Standard -> "S"
  | Default -> "D"
