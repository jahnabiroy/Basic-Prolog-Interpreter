%{
    open Backend;;
%}

%token <string> VAR NVAR
%token <int> NUM
%token LPAREN RPAREN LB RB AND ENDL COND EOF OR OFC SLICE

%left AND OR
%nonassoc ENDL OFC SLICE
%start program goal
%type <Backend.program> program
%type <Backend.goal> goal
%%

program:
    EOF                                 {[]}
  | clause_list EOF                     {$1}
;

clause_list:
    clause                              {[$1]}
  | clause clause_list                  {($1)::$2}
;

clause:
    atom ENDL                           {Fact(H($1))}
  | atom COND atom_list ENDL            {Rule(H($1), B($3))}
;

goal:
    atom_list ENDL                      {G($1)}
;

atom_list:
    atom                                {[$1]}
  | atom AND atom_list                {($1)::$3}
;

atom:
  | NVAR                                {A($1, [])}
  | NVAR LPAREN term_list RPAREN        {A($1, $3)}
  | OFC                                 {A("cut", [])}
;

term_list:
    term                                {[$1]}
  | term AND term_list                {($1)::$3}
;

term:
    LPAREN term RPAREN                  {$2}
  | VAR                                 {V($1)}
  | NVAR                                {ATOM($1, [])}
  | NUM                                 {CInt($1)}
  | NVAR LPAREN term_list RPAREN        {ATOM($1, $3)}
  | list                                {$1}
;

list:
    LB RB                               {ATOM("empty_list", [])}
  | LB list_body RB                     {$2}
;

list_body:
    term                                 {ATOM("list", [$1; ATOM("empty_list", [])])}
  | term AND list_body                 {ATOM("list", [$1; $3])}
  | term SLICE term                       {ATOM("list", [$1; $3])}
;
