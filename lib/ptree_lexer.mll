
{
  open Ptree_parser
  open Lexing

  exception Bad_char of char
}

rule token = parse
| ' ' | '\t' | '\n'                 { token lexbuf }
| ','                               { COMMA }
| '('                               { LP }
| ')'                               { RP }
(* | 'C' | 'S' | 'D' as c              { META (String.make 1 c) } *)
| ['a'-'z' 'A'-'Z' '_' '0'-'9' ]+ as s       { STRING s }
| _ as c                            { raise (Bad_char c) }
and rule_tail acc = parse
  | eof                             { acc }
  | _* as str                       { rule_tail (acc ^ str) lexbuf }
