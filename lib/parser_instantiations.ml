
(* module Rule_list_parser = struct                                            *)
(*   type parser_token = Rule_list_parser.token                                *)
(*   type result = int * string * Rule.t list                                  *)
(*   let input = Rule_list_parser.main                                         *)
(*   let lexer_token = Rule_list_lexer.token                                   *)
(*   let rule_tail = Rule_list_lexer.rule_tail                                 *)
(* end                                                                         *)

(* module Rule_list_parser_utils = Parser_utils_functor.Make(Rule_list_parser) *)

module Rule_tree_parser = struct
  type parser_token = Rule_tree_parser.token
  type result = int * string * string * (Boolean_operator.t , Rule.t) Tree.t
  let input = Rule_tree_parser.main
  let lexer_token = Rule_tree_lexer.token
  let rule_tail = Rule_tree_lexer.rule_tail
end

module Rule_tree_parser_utils = Parser_utils.Make(Rule_tree_parser)
    
module Ptree_parser = struct
  type parser_token = Ptree_parser.token
  type result = (string * string * string * string) Ptree.t
  let input = Ptree_parser.ptree
  let lexer_token = Ptree_lexer.token
  let rule_tail = Ptree_lexer.rule_tail
end

module Ptree_parser_utils = Parser_utils.Make(Ptree_parser)

