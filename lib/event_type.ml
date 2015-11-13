
open Printf

type t =
| All
| Anomaly
| Normal
| Unknown
with compare, sexp, bin_io

let of_string string =
  match string with
  | "all" -> All
  | "anomaly" -> Anomaly
  | "normal" -> Normal
  | "unknown" -> Unknown
  | _ -> failwith (sprintf "Event_type: of_string: invalid string: %s" string)

let to_string t =
  match t with
  | All -> "all"
  | Anomaly -> "anomaly"
  | Normal -> "normal"
  | Unknown -> "unknown"
