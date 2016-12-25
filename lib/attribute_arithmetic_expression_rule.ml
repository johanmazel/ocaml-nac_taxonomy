
open Printf

open Sexplib.Std
open Bin_prot.Std

(* let debug_enabled = ref false     *)

(* let set_debug bool = debug_enabled := bool*)

(* let debug fmt =   *)
(*   Printf.kprintf  *)
(*     (     *)
(*       if !debug_enabled then      *)
(*         (fun s -> Format.printf "[Rule]: %s@." s) *)
(*       else*)
(*         ignore    *)
(*     )     *)
(*     fmt   *)

type t =
  {
    feature_arithmetic_expression : Feature_arithmetic_expression.t;
    attribute_comparison_criteria : Attribute_comparison_criteria.t;
    threshold : float;
  }
[@@deriving compare, sexp, bin_io]

let new_t
    feature_arithmetic_expression
    attribute_comparison_criteria
    threshold
    =
  {
    feature_arithmetic_expression;
    attribute_comparison_criteria;
    threshold;
  }

let to_string t =
  sprintf
    "Rule on %s %s %f"
    (Feature_arithmetic_expression.to_string t.feature_arithmetic_expression)
    (Attribute_comparison_criteria.to_string t.attribute_comparison_criteria)
    t.threshold

let test_value t value =
  match t.attribute_comparison_criteria with
  | Attribute_comparison_criteria.Greater -> value > t.threshold
  | Attribute_comparison_criteria.Greater_or_equal -> value >= t.threshold
  | Attribute_comparison_criteria.Lower -> value < t.threshold
  | Attribute_comparison_criteria.Lower_or_equal -> value <= t.threshold

let fold 
    f_leaf
    f_node
    t
    =
  Feature_arithmetic_expression.fold 
    f_leaf
    f_node
    t.feature_arithmetic_expression

