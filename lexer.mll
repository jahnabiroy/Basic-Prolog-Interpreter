{
  open Parser;;
}

rule token = parse
    eof                   {EOF}
  | [' ' '\t' '\n']+      {token lexbuf}
  | '.'                   {ENDL}
  | ['A'-'Z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*  as v {VAR(v)}
  | ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as c {NVAR(c)} 
  | '0'|['1'-'9']['0'-'9']*     as n {NUM(int_of_string n)}
  | '('                   {LPAREN}
  | ')'                   {RPAREN}
  | '['                   {LB}
  | ']'                   {RB}
  | ','                   {AND}
  | ';'                   {OR}
  | ":-"                  {COND}
  | '|'                   {SLICE}
  | '!'                   {OFC}
  | '%'                   {line_comment lexbuf}
  | "/*"                  {block_comment 0 lexbuf}

and line_comment = parse
    eof                   {EOF}
  | '\n'                  {token lexbuf}
  |   _                   {line_comment lexbuf}

and block_comment depth = parse
    eof                   {EOF}
  | "*/"                  {if depth = 0 then token lexbuf else block_comment (depth-1) lexbuf}
  | "/*"                  {block_comment (depth+1) lexbuf}
  |  _                    {block_comment depth lexbuf}
