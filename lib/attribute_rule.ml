
open Printf

open Sexplib.Std
open Bin_prot.Std

type t =
  {
    feature : Feature.t;
    attribute_comparison_criteria : Attribute_comparison_criteria.t;
    threshold : float;
  }
[@@deriving compare, sexp, bin_io]

let new_t
    feature
    attribute_comparison_criteria
    threshold
    =
  {
    feature;
    attribute_comparison_criteria;
    threshold;
  }

let to_string t =
  sprintf
    "Rule on %s %s %f"
    (Feature.to_string t.feature)
    (Attribute_comparison_criteria.to_string t.attribute_comparison_criteria)
    t.threshold

let test_value t value =
  match t.attribute_comparison_criteria with
  | Attribute_comparison_criteria.Greater -> value > t.threshold
  | Attribute_comparison_criteria.Greater_or_equal -> value >= t.threshold
  | Attribute_comparison_criteria.Lower -> value < t.threshold
  | Attribute_comparison_criteria.Lower_or_equal -> value <= t.threshold
