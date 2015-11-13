
open Printf

type t =
| None
| Src
| Dst
| Src_aggr
| Dst_aggr
with compare, sexp, bin_io

let of_string string =
  match string with
  | "none" -> None
  | "src" -> Src
  | "dst" -> Dst
  | "src_aggr" -> Src_aggr
  | "dst_aggr" -> Dst_aggr
  | _ -> failwith (sprintf "Anomaly_signature_type: of_string: invalid string: %s" string)

let to_string t =
  match t with
  | None -> "none"
  | Src -> "src"
  | Dst -> "dst"
  | Src_aggr -> "src_aggr"
  | Dst_aggr -> "dst_aggr"

let (=) t1 t2 = compare t1 t2 = 0

let to_list () =
  [
    None;
    Src;
    Dst;
    Src_aggr;
    Dst_aggr;
  ]
