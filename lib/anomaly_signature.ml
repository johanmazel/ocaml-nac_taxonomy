
open Printf

open Sexplib.Std
open Bin_prot.Std

open Parser_instantiations

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Anomaly_signature]: %s@." s)
      else
  ignore
    )
    fmt

type t =
  {
    indice : int;
    name : string;
    short_name : string;
    boolean_operator_rule_tree : (Boolean_operator.t , Rule.t) Tree.t;
    (* boolean_operator_rule_tree : (Boolean_operator.t , Rule.t) Ptree.t; *)
  }
[@@deriving compare, sexp, bin_io]

let new_t
    indice
    name
    short_name
    boolean_operator_rule_tree
    =
  {
    indice;
    name;
    short_name;
    boolean_operator_rule_tree;
  }

let to_short_name t = t.short_name

let to_string
    (* to_string_mode *)
    t
    =
  (* match to_string_mode with *)
  (* | To_string_mode.Command -> *)
  (*   sprintf *)
  (*     "%s" *)
  (*     t.short_name *)
  (* | To_string_mode.Simple -> *)
  (*   sprintf *)
  (*     "%d - %s" *)
  (*     t.indice *)
  (*     t.name *)
  (* | To_string_mode.Normal -> *)
    sprintf
      "Anomaly_signature %d - %s - %s"
      t.indice
      t.name
      t.short_name

(* sprintf                                                                      *)
(*   "Anomaly_signature %d - %s:\n%s"                                           *)
(*   t.indice                                                                   *)
(*   t.name                                                                     *)
(*   (Feature_rule_container.to_string to_string_mode t.feature_rule_container) *)

(* let length_metric_rule t = Metric_rule_container.length t.metric_rule_container *)

(* TODO: write proper function that search in taxonomy *)
let of_string_empty
    string
    =
  (* debug "of_string_empty: call"; *)

  (* let list = Str.split (Str.regexp "-") string in *)
  (* if (List.length list <> 2) then *)
  (*   ( *)
  (*     failwith *)
  (*       (sprintf *)
  (*          "Anomaly_signature: of_string_empty: invalid string: %s" *)
  (*          string *)
  (*       ); *)
  (*   ); *)

  (* let indice_string = String.trim (List.nth list 0) in *)
  (* let indice =  *)
  (*   try *)
  (*     int_of_string indice_string *)
  (*   with *)
  (*   | Failure message -> *)
  (*     failwith  *)
  (*       (sprintf *)
  (*          "Anomaly_signature: of_string_empty: \"%s\" problem for \"%s\"" *)
  (*          message *)
  (*          indice_string *)
  (*       ) *)
  (* in *)

  (* let short_name = String.trim (List.nth list 1) in *)

  let short_name = string in

  (* debug "of_string_empty: %d \"%s\"" indice short_name; *)

  let boolean_operator_rule_tree =
    Tree.Node (Boolean_operator.AND,[])
  in

  (* debug "of_string_empty: end"; *)

  new_t
    0
    ""
    short_name
    boolean_operator_rule_tree

let of_filename
    global_feature_container
    global_metric_container
    filepath
    =
  (* let rule_list = Parser_utils.parse filename in *)
  let indice , name , short_name , boolean_operator_rule_tree = Rule_tree_parser_utils.parse filepath in
  

  let filename = Filename.basename filepath in

  if String.compare name filename <> 0 then
    (
      print_endline 
        (sprintf
           "Anomaly_signature: inconsistent filename and anomaly name: %s - %s"
           filename
           name
        );

      assert(false)
    );

  (* TODO: check existence of metric and feature *)
  (* Tree.iter *)
  (* (         *)
  
  (*   )       *)
  
  (* let (feature_rule_list, metric_rule_list) =                                                             *)
  (*   List.fold_left                                                                                        *)
  (*     (fun (feature_rule_list_acc , metric_rule_list_acc) rule ->                                         *)
  (*           match rule with                                                                               *)
  (*           | Rule.Feature feature_rule -> (feature_rule :: feature_rule_list_acc , metric_rule_list_acc) *)
  (*           | Rule.Metric metric_rule -> (feature_rule_list_acc , metric_rule :: metric_rule_list_acc)    *)
  (*     )                                                                                                   *)
  (*     ([],[])                                                                                             *)
  (*     rule_list                                                                                           *)
  (* in                                                                                                      *)
  
  (* let feature_rule_container =                                                                            *)
  (*   Feature_rule_container.of_feature_rule_list                                                           *)
  (*     global_feature_container                                                                            *)
  (*     feature_rule_list                                                                                   *)
  (* in                                                                                                      *)
  
  (* let metric_value_rule_container =                                                                       *)
  (*   Metric_rule_container.of_int_metric_rule_list                                                         *)
  (*     global_metric_container                                                                             *)
  (*     metric_rule_list                                                                                    *)
  (* in                                                                                                      *)
  
  new_t
    indice
    name
    short_name
    (* feature_rule_container      *)
    (* metric_value_rule_container *)
    boolean_operator_rule_tree

(* let iter_feature_rule         *)
(*     f                         *)
(*     t                         *)
(* =                             *)
(*   Feature_rule_container.iter *)
(*     f                         *)
(*     t.feature_rule_container  *)

(* let fold_feature_rule         *)
(*     f                         *)
(*     t                         *)
(*     acc                       *)
(* =                             *)
(*   Feature_rule_container.fold *)
(*     f                         *)
(*     t.feature_rule_container  *)
(*     acc                       *)

(* let fold_metric_rule         *)
(*     f                        *)
(*     t                        *)
(*     acc                      *)
(* =                            *)
(*   Metric_rule_container.fold *)
(*     f                        *)
(*     t.metric_rule_container  *)
(*     acc                      *)

let fold
    f_leaf
    f_node
    acc
    t
    =
  Tree.fold
    f_leaf
    f_node
    acc
    t.boolean_operator_rule_tree

let fold_wo_acc
    f_leaf
    f_node
    t
    =
  Tree.fold_wo_acc
    f_leaf
    f_node
    t.boolean_operator_rule_tree

let is_empty t =
  match t.boolean_operator_rule_tree with
  | Tree.Leaf _ -> 
    (* print_endline *)
    (*   (sprintf *)
    (*    "Anomaly_signature: is_empty: only a leaf in tree:\n%s" *)
    (*    (to_string To_string_mode.Normal t) *)
    (*   ); *)
    (* assert(false) *)
    false
  | Tree.Node (boolean_operator, list) ->
    if List.length list = 0 then
      (
    assert(Boolean_operator.compare Boolean_operator.AND boolean_operator = 0);
    true
      )
    else
      false
  
