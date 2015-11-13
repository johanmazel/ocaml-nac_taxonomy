
%token COMMA

%token LP RP
%token LB RB

%token <int> INT
%token <string> STRING
%token <float> FLOAT

%token <string> FEATURE_COMPARISON_OPERATOR METRIC_COMPARISON_OPERATOR ARITHMETIC_OPERATOR

%token FIRST_LINE_EOL
%token EOF

%start main

%type <int * string * string * (Boolean_operator.t , Rule.t) Tree.t> main

%%

main:
  first_line bool_expression EOF                                               { (fst $1 , (fst (snd $1)) , (snd (snd $1)) , $2) }
  
first_line:
    | INT STRING STRING FIRST_LINE_EOL                                         { ($1, ($2, $3)) }
  ;

bool_expression:
    | LP RP                                                                    { Tree.Node (Boolean_operator.AND , [ ]) }
    | rule                                                                     { Tree.Leaf $1 }
    (* | LP rule RP                                                               { Tree.Leaf $2 } *)
    | LP bool_expression RP                                                    { Tree.Node (Boolean_operator.AND , [ $2 ]) }
    | bo = STRING; LP; children = separated_list(COMMA, bool_expression); RP   { Tree.Node (Boolean_operator.of_string bo, children) }
   ;

rule:
    | feature_rule                                                             { Rule.Attribute_arithmetic_expression $1 }
    | metric_rule                                                              { Rule.Value $1 }
  ;

feature_rule:
  binary_tree_node_root FEATURE_COMPARISON_OPERATOR FLOAT                      { Attribute_arithmetic_expression_rule.new_t $1 (Attribute_comparison_criteria.of_string $2) $3 }
  
binary_tree_node_root:
    | feature                                                                  { Binary_tree.Leaf (Feature.new_t (fst $1) (snd $1)) }
    | binary_tree_node ARITHMETIC_OPERATOR binary_tree_node                    { Binary_tree.Node ((Arithmetic_operator.of_string $2), $1, $3) }
    | LB binary_tree_node ARITHMETIC_OPERATOR binary_tree_node RB              { Binary_tree.Node ((Arithmetic_operator.of_string $3), $2, $4) }

binary_tree_node:
    | feature                                                                  { Binary_tree.Leaf (Feature.new_t (fst $1) (snd $1)) }
    | LB binary_tree_node ARITHMETIC_OPERATOR binary_tree_node RB              { Binary_tree.Node ((Arithmetic_operator.of_string $3), $2, $4) }


metric_rule:
feature METRIC_COMPARISON_OPERATOR INT                                         { Value_rule.new_int_t (Feature.new_t (fst $1) (snd $1)) (Value_comparison_criteria.of_string $2) $3 }
 ;

feature:
  INT STRING                                                                   { ($1, $2) }
  ;
  
