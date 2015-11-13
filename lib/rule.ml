
open Sexplib.Std
open Bin_prot.Std

type t =
| Attribute of Attribute_rule.t
| Attribute_arithmetic_expression of Attribute_arithmetic_expression_rule.t
| Value of int Value_rule.t
with compare, sexp, bin_io

let to_string t =
  match t with
  | Attribute attribute_rule -> Attribute_rule.to_string attribute_rule
  | Attribute_arithmetic_expression attribute_arithmetic_expression_rule ->
    Attribute_arithmetic_expression_rule.to_string
      attribute_arithmetic_expression_rule
  | Value value_rule -> Value_rule.to_string value_rule
