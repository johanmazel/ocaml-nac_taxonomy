{
 open Rule_tree_parser        (* The type token is defined in parser.mli *)

 exception LexError of string
}

rule token = parse
  [' ' '\t' '\n']                                              { token lexbuf }     (* skip blanks *)
| ":\n"                                                        { FIRST_LINE_EOL }
| ','                                                          { COMMA }
| '('                                                          { LP }
| ')'                                                          { RP }
| '['                                                          { LB }
| ']'                                                          { RB }
| ['0'-'9']+ as lxm                                            { INT(int_of_string lxm) }
| ['A'-'Z' 'a'-'z' '_' '0'-'9' '&' '|' '[' ']' ]+ as string    { STRING(string) }
| ['0'-'9']+'.'['0'-'9']* as string                            { FLOAT(float_of_string string) }
| ['>' '<' ] ['=']? as string                                  { FEATURE_COMPARISON_OPERATOR(string) }
| '=' | "<>" as string                                         { METRIC_COMPARISON_OPERATOR(string) }
| ['+' '-' '*' '/'] as string                                  { ARITHMETIC_OPERATOR(Char.escaped string) }
| eof                                                          { EOF }
and rule_tail acc = parse
  | eof                                                        { acc }
  | _* as str                                                  { rule_tail (acc ^ str) lexbuf }
