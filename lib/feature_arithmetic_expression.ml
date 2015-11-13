
open Bin_prot.Std

type t = (Arithmetic_operator.t, Feature.t) Binary_tree.t
with compare, sexp, bin_io

let to_string t =
  Binary_tree.to_string
    Feature.to_string
    (fun arithmetic_operator s1 s2 ->
      s1 ^ Arithmetic_operator.to_string arithmetic_operator ^ s2
    )
    t

let fold 
    f_leaf
    f_node
    t
    =
  Binary_tree.fold
    f_leaf
    f_node
    t

