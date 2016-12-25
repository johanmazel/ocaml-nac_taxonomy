
open Printf

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Value_rule]: %s@." s)
      else
        ignore
    )
    fmt

type 'a t =
  {
    feature : Feature.t;
    value_comparison_criteria : Value_comparison_criteria.t;
    value_variant : Value_variant.t;
  }
[@@deriving compare, sexp, bin_io]

let new_t
    feature
    value_comparison_criteria
    value_variant
    =
  {
    feature;
    value_comparison_criteria;
    value_variant;
  }

let new_int_t
    metric
    metric_comparison_criteria
    metric_value
    =
  new_t
    metric
    metric_comparison_criteria
    (Value_variant.Int metric_value)

let new_int32_t
    metric
    metric_comparison_criteria
    metric_value
    =
  new_t
    metric
    metric_comparison_criteria
    (Value_variant.Int metric_value)

let to_string t =
  sprintf
    "Rule on %s %s %s"
    (Feature.to_string t.feature)
    (Value_comparison_criteria.to_string t.value_comparison_criteria)
    (Value_variant.to_string t.value_variant)

let test_value t value =
  match t.value_comparison_criteria with
  | Value_comparison_criteria.Equal -> value = t.value_variant
  | Value_comparison_criteria.Different -> value <> t.value_variant
