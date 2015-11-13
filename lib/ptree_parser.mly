
%{
  open Ptree
%}

%token COMMA LP RP
%token <string> STRING
(* %token <string> META *)

%start <(string * string * string * string) Ptree.t> ptree

%%

ptree:
  event_type = STRING anomaly_signature_type = STRING label_type = STRING label = STRING LP children = separated_list(COMMA, ptree) RP          { Ptree.Node ((event_type, anomaly_signature_type, label_type, label) , children) }
